;; BountyChain: Decentralized Task Bounty & Reward Protocol
;; Version: 1.0.0
;; A protocol for creating task bounties, staking rewards, and distributing payments to contributors

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u1))
(define-constant ERR-BOUNTY-NOT-FOUND (err u2))
(define-constant ERR-INVALID-REWARD (err u3))
(define-constant ERR-INVALID-DEADLINE (err u4))
(define-constant ERR-INVALID-TITLE (err u5))
(define-constant ERR-INVALID-REQUIREMENTS (err u6))
(define-constant ERR-BOUNTY-INACTIVE (err u7))
(define-constant ERR-ALREADY-CLAIMED (err u8))
(define-constant ERR-NOT-CLAIMED (err u9))
(define-constant ERR-INSUFFICIENT-FUNDS (err u10))
(define-constant ERR-WORK-NOT-SUBMITTED (err u11))
(define-constant ERR-ALREADY-REWARDED (err u12))
(define-constant ERR-INVALID-PRIORITY (err u13))
(define-constant ERR-INVALID-COMPLEXITY (err u14))
(define-constant ERR-DEADLINE-PASSED (err u15))
(define-constant ERR-INVALID-SUBMISSION (err u16))

;; Constants
(define-constant MIN-REWARD u500000) ;; 0.5 STX minimum
(define-constant MAX-REWARD u500000000000) ;; 500k STX maximum
(define-constant MIN-DEADLINE u3600) ;; 1 hour minimum
(define-constant MAX-DEADLINE u15552000) ;; 6 months maximum
(define-constant PROTOCOL-FEE-PERCENT u3) ;; 3% protocol fee
(define-constant COMPLETION-THRESHOLD u90) ;; 90% minimum quality for reward

;; Data variables
(define-data-var next-bounty-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var protocol-vault principal tx-sender)
(define-data-var total-protocol-revenue uint u0)

;; Bounty data structure
(define-map bounties
    uint
    {
        creator: principal,
        title: (string-utf8 100),
        requirements: (string-utf8 500),
        priority: (string-utf8 20),
        complexity: (string-utf8 10),
        reward: uint,
        bond-amount: uint,
        deadline: uint,
        is-active: bool,
        total-claims: uint,
        total-completed: uint,
        created-at: uint
    })

;; Claim data structure
(define-map claims
    uint
    {
        hunter: principal,
        bounty-id: uint,
        claimed-at: uint,
        deadline-at: uint,
        quality-score: uint,
        is-submitted: bool,
        is-rewarded: bool,
        bond-locked: uint
    })

;; Hunter claims by bounty
(define-map hunter-bounty-claims
    { hunter: principal, bounty-id: uint }
    uint)

;; Completion records
(define-map completions
    { hunter: principal, bounty-id: uint }
    {
        completed-at: uint,
        final-quality: uint,
        submission-hash: (string-utf8 64)
    })

;; Private validation functions
(define-private (validate-priority (priority (string-utf8 20)))
    (or 
        (is-eq priority u"Critical")
        (is-eq priority u"High")
        (is-eq priority u"Medium")
        (is-eq priority u"Low")
        (is-eq priority u"Enhancement")
        (is-eq priority u"Research")
        (is-eq priority u"Documentation")
        (is-eq priority u"Testing")
    ))

(define-private (validate-complexity (complexity (string-utf8 10)))
    (or 
        (is-eq complexity u"Simple")
        (is-eq complexity u"Moderate")
        (is-eq complexity u"Complex")
        (is-eq complexity u"Expert")
    ))

(define-private (validate-text-length (text (string-utf8 500)) (min-length uint) (max-length uint))
    (let 
        (
            (text-length (len text))
        )
        (and 
            (>= text-length min-length)
            (<= text-length max-length)
        )
    ))

(define-private (calculate-protocol-fee (amount uint))
    (/ (* amount PROTOCOL-FEE-PERCENT) u100))

(define-private (calculate-creator-refund (amount uint))
    (- amount (calculate-protocol-fee amount)))

(define-private (validate-bond-amount (bond-amount uint))
    (and (>= bond-amount u0) (<= bond-amount u50000000000))) ;; Max 50k STX bond

(define-private (validate-submission-hash (submission-hash (string-utf8 64)))
    (and (>= (len submission-hash) u32) (<= (len submission-hash) u64)))

;; Public functions

;; Create a new bounty
(define-public (create-bounty 
    (title (string-utf8 100))
    (requirements (string-utf8 500))
    (priority (string-utf8 20))
    (complexity (string-utf8 10))
    (reward uint)
    (bond-amount uint)
    (deadline uint))
    (let
        (
            (bounty-id (var-get next-bounty-id))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        ;; Validate inputs
        (asserts! (validate-text-length title u5 u100) ERR-INVALID-TITLE)
        (asserts! (validate-text-length requirements u20 u500) ERR-INVALID-REQUIREMENTS)
        (asserts! (validate-priority priority) ERR-INVALID-PRIORITY)
        (asserts! (validate-complexity complexity) ERR-INVALID-COMPLEXITY)
        (asserts! (and (>= reward MIN-REWARD) (<= reward MAX-REWARD)) ERR-INVALID-REWARD)
        (asserts! (and (>= deadline MIN-DEADLINE) (<= deadline MAX-DEADLINE)) ERR-INVALID-DEADLINE)
        (asserts! (validate-bond-amount bond-amount) ERR-INVALID-REWARD)
        
        ;; Create bounty
        (map-set bounties bounty-id {
            creator: tx-sender,
            title: title,
            requirements: requirements,
            priority: priority,
            complexity: complexity,
            reward: reward,
            bond-amount: bond-amount,
            deadline: deadline,
            is-active: true,
            total-claims: u0,
            total-completed: u0,
            created-at: current-time
        })
        
        (var-set next-bounty-id (+ bounty-id u1))
        (ok bounty-id)
    ))

;; Claim a bounty with bond
(define-public (claim-bounty (bounty-id uint))
    (let
        (
            (bounty (unwrap! (map-get? bounties bounty-id) ERR-BOUNTY-NOT-FOUND))
            (claim-id (var-get next-claim-id))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
            (deadline-at (+ current-time (get deadline bounty)))
            (total-cost (+ (get reward bounty) (get bond-amount bounty)))
            (protocol-fee (calculate-protocol-fee (get reward bounty)))
            (creator-amount (calculate-creator-refund (get reward bounty)))
        )
        ;; Validate bounty is active
        (asserts! (get is-active bounty) ERR-BOUNTY-INACTIVE)
        
        ;; Check if already claimed
        (asserts! (is-none (map-get? hunter-bounty-claims { hunter: tx-sender, bounty-id: bounty-id })) ERR-ALREADY-CLAIMED)
        
        ;; Transfer reward to protocol vault and protocol fee
        (try! (stx-transfer? creator-amount tx-sender (get creator bounty)))
        (try! (stx-transfer? protocol-fee tx-sender (var-get protocol-vault)))
        
        ;; Lock bond amount (simulated by requiring balance)
        (asserts! (>= (stx-get-balance tx-sender) (get bond-amount bounty)) ERR-INSUFFICIENT-FUNDS)
        
        ;; Create claim
        (map-set claims claim-id {
            hunter: tx-sender,
            bounty-id: bounty-id,
            claimed-at: current-time,
            deadline-at: deadline-at,
            quality-score: u0,
            is-submitted: false,
            is-rewarded: false,
            bond-locked: (get bond-amount bounty)
        })
        
        ;; Map hunter to claim
        (map-set hunter-bounty-claims { hunter: tx-sender, bounty-id: bounty-id } claim-id)
        
        ;; Update bounty stats
        (map-set bounties bounty-id (merge bounty { total-claims: (+ (get total-claims bounty) u1) }))
        
        ;; Update protocol revenue
        (var-set total-protocol-revenue (+ (var-get total-protocol-revenue) protocol-fee))
        (var-set next-claim-id (+ claim-id u1))
        
        (ok claim-id)
    ))

;; Update work progress
(define-public (update-quality-score (bounty-id uint) (quality-score uint))
    (let
        (
            (claim-id (unwrap! (map-get? hunter-bounty-claims { hunter: tx-sender, bounty-id: bounty-id }) ERR-NOT-CLAIMED))
            (claim (unwrap! (map-get? claims claim-id) ERR-NOT-CLAIMED))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        ;; Validate claim is active
        (asserts! (< current-time (get deadline-at claim)) ERR-DEADLINE-PASSED)
        (asserts! (<= quality-score u100) ERR-INVALID-SUBMISSION)
        (asserts! (>= quality-score (get quality-score claim)) ERR-INVALID-SUBMISSION)
        
        ;; Update quality score
        (map-set claims claim-id (merge claim { 
            quality-score: quality-score,
            is-submitted: (>= quality-score u100)
        }))
        
        (ok true)
    ))

;; Issue reward for completion
(define-public (issue-reward (bounty-id uint) (submission-hash (string-utf8 64)))
    (let
        (
            (claim-id (unwrap! (map-get? hunter-bounty-claims { hunter: tx-sender, bounty-id: bounty-id }) ERR-NOT-CLAIMED))
            (claim (unwrap! (map-get? claims claim-id) ERR-NOT-CLAIMED))
            (bounty (unwrap! (map-get? bounties bounty-id) ERR-BOUNTY-NOT-FOUND))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
            (validated-bounty-id (get bounty-id claim))
            (validated-hash submission-hash)
        )
        ;; Additional validations
        (asserts! (validate-submission-hash submission-hash) ERR-INVALID-REQUIREMENTS)
        (asserts! (is-eq bounty-id validated-bounty-id) ERR-BOUNTY-NOT-FOUND)
        
        ;; Validate submission and quality
        (asserts! (get is-submitted claim) ERR-WORK-NOT-SUBMITTED)
        (asserts! (>= (get quality-score claim) COMPLETION-THRESHOLD) ERR-WORK-NOT-SUBMITTED)
        (asserts! (not (get is-rewarded claim)) ERR-ALREADY-REWARDED)
        
        ;; Issue reward record
        (map-set completions { hunter: tx-sender, bounty-id: validated-bounty-id } {
            completed-at: current-time,
            final-quality: (get quality-score claim),
            submission-hash: validated-hash
        })
        
        ;; Update claim
        (map-set claims claim-id (merge claim { is-rewarded: true }))
        
        ;; Update bounty stats
        (map-set bounties validated-bounty-id (merge bounty { total-completed: (+ (get total-completed bounty) u1) }))
        
        ;; Return bond to hunter (simulated)
        (ok true)
    ))

;; Deactivate bounty (creator only)
(define-public (deactivate-bounty (bounty-id uint))
    (let
        (
            (bounty (unwrap! (map-get? bounties bounty-id) ERR-BOUNTY-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get creator bounty)) ERR-NOT-AUTHORIZED)
        (map-set bounties bounty-id (merge bounty { is-active: false }))
        (ok true)
    ))

;; Read-only functions
(define-read-only (get-bounty (bounty-id uint))
    (map-get? bounties bounty-id))

(define-read-only (get-claim (claim-id uint))
    (map-get? claims claim-id))

(define-read-only (get-hunter-claim (hunter principal) (bounty-id uint))
    (match (map-get? hunter-bounty-claims { hunter: hunter, bounty-id: bounty-id })
        claim-id (map-get? claims claim-id)
        none
    ))

(define-read-only (get-completion (hunter principal) (bounty-id uint))
    (map-get? completions { hunter: hunter, bounty-id: bounty-id }))

(define-read-only (is-hunter-rewarded (hunter principal) (bounty-id uint))
    (is-some (map-get? completions { hunter: hunter, bounty-id: bounty-id })))

(define-read-only (get-bounty-stats (bounty-id uint))
    (match (map-get? bounties bounty-id)
        bounty {
            total-claims: (get total-claims bounty),
            total-completed: (get total-completed bounty),
            success-rate: (if (> (get total-claims bounty) u0)
                (/ (* (get total-completed bounty) u100) (get total-claims bounty))
                u0
            )
        }
        { total-claims: u0, total-completed: u0, success-rate: u0 }
    ))

(define-read-only (get-protocol-stats)
    {
        total-bounties: (- (var-get next-bounty-id) u1),
        total-claims: (- (var-get next-claim-id) u1),
        total-protocol-revenue: (var-get total-protocol-revenue),
        protocol-vault: (var-get protocol-vault)
    })

(define-read-only (calculate-bounty-cost (bounty-id uint))
    (match (map-get? bounties bounty-id)
        bounty {
            reward: (get reward bounty),
            bond: (get bond-amount bounty),
            total: (+ (get reward bounty) (get bond-amount bounty)),
            protocol-fee: (calculate-protocol-fee (get reward bounty)),
            creator-amount: (calculate-creator-refund (get reward bounty))
        }
        { reward: u0, bond: u0, total: u0, protocol-fee: u0, creator-amount: u0 }
    ))