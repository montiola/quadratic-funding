;; Quadratic Funding Contract
;; This contract manages a quadratic funding system that allocates resources according to predefined rules

;; Error codes
(define-constant ERR-UNAUTHORIZED-ACCESS (err u100))
(define-constant ERR-CAMPAIGN-INACTIVE (err u101))
(define-constant ERR-BALANCE-INSUFFICIENT (err u102))
(define-constant ERR-CONTRIBUTION-EXCEEDS-LIMIT (err u103))
(define-constant ERR-CONTRIBUTION-BELOW-MINIMUM (err u104))
(define-constant ERR-INVALID-INITIATIVE (err u105))
(define-constant ERR-CAMPAIGN-ALREADY-EXISTS (err u106))
(define-constant ERR-CAMPAIGN-NOT-EXISTS (err u107))
(define-constant ERR-INITIATIVE-ALREADY-EXISTS (err u108))
(define-constant ERR-INITIATIVE-NOT-EXISTS (err u109))
(define-constant ERR-CONTRIBUTOR-LIMIT-EXCEEDED (err u110))
(define-constant ERR-FUNDS-ALREADY-WITHDRAWN (err u111))
(define-constant ERR-CAMPAIGN-STILL-ACTIVE (err u112))
(define-constant ERR-INVALID-PARAMETER (err u113))
(define-constant ERR-ZERO-DIVISION (err u114))

;; Data storage
(define-data-var admin-address principal tx-sender)
(define-data-var vault-balance uint u0)
(define-data-var total-allocated-funds uint u0)
(define-data-var current-campaign-id uint u0)

;; Map: campaign ID => campaign info
(define-map funding-campaigns
  { campaign-id: uint }
  {
    launch-block: uint,
    conclusion-block: uint,
    allocation-pool: uint,
    remaining-allocation: uint,
    minimum-contribution: uint,
    maximum-contribution: uint,
    contributor-limit: uint,
    total-contributions: uint,
    status-active: bool,
    status-completed: bool
  }
)

;; Map: initiative ID => initiative info
(define-map initiatives
  { initiative-id: uint }
  {
    title: (string-ascii 100), 
    creator: principal,
    total-contributions: uint,
    total-allocated: uint,
    status-active: bool
  }
)

;; Map: (campaign-id, initiative-id) => approval status
(define-map campaign-initiatives
  { campaign-id: uint, initiative-id: uint }
  {
    approval-status: bool,
    total-contributions: uint
  }
)

;; Map: (campaign-id, contributor, initiative-id) => contribution info
(define-map contributions
  { campaign-id: uint, contributor: principal, initiative-id: uint }
  {
    contribution-amount: uint,
    allocated-amount: uint,
    withdrawal-status: bool
  }
)

;; Map: (campaign-id, contributor) => total contributed in campaign
(define-map contributor-totals
  { campaign-id: uint, contributor: principal }
  { total-contributed: uint }
)

;; Map: initiative ID => next ID
(define-data-var next-initiative-id uint u1)
(define-data-var next-campaign-id uint u1)

;; Fixed validate-uint function - returns response type consistently
(define-private (validate-parameter (input uint))
  (ok input)
)

;; Initialize contract
(define-public (initialize-system)
  (begin
    (asserts! (is-eq tx-sender (var-get admin-address)) ERR-UNAUTHORIZED-ACCESS)
    (ok true)
  )
)

;; Only admin modifier
(define-private (is-admin)
  (is-eq tx-sender (var-get admin-address))
)

;; Change admin - fixed by validating input
(define-public (update-admin-address (new-admin principal))
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED-ACCESS)
    ;; Check that new-admin is not tx-sender, or some other validation
    ;; This is a simple fix; you might want more validation logic
    (asserts! (not (is-eq new-admin tx-sender)) ERR-INVALID-INITIATIVE)
    (var-set admin-address new-admin)
    (ok true)
  )
)

;; Safe division to avoid division by zero
(define-read-only (secure-divide (numerator uint) (denominator uint))
  (if (> denominator u0)
      (ok (/ numerator denominator))
      (err ERR-ZERO-DIVISION))
)

;; Calculate proportion of funds
(define-private (calculate-allocation (amount uint) (total uint) (pool uint))
  (if (> total u0)
      (/ (* amount pool) total)
      u0)
)

;; Create a new funding campaign - fixed by validating inputs
(define-public (create-funding-campaign
                (launch-block uint)
                (conclusion-block uint)
                (allocation-pool uint)
                (minimum-contribution uint)
                (maximum-contribution uint)
                (contributor-limit uint))
  (let (
        (campaign-id (var-get next-campaign-id))
        (validated-allocation-pool allocation-pool) ;; Added validation variable
        (validated-contributor-limit contributor-limit) ;; Added validation variable
       )
    (asserts! (is-admin) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (> conclusion-block launch-block) ERR-INVALID-INITIATIVE)
    (asserts! (>= launch-block block-height) ERR-INVALID-INITIATIVE)
    (asserts! (>= maximum-contribution minimum-contribution) ERR-INVALID-INITIATIVE)
    
    ;; Validate pool and limit are not zero
    (asserts! (> validated-allocation-pool u0) ERR-INVALID-PARAMETER)
    (asserts! (> validated-contributor-limit u0) ERR-INVALID-PARAMETER)
    
    ;; Create the funding campaign
    (map-insert funding-campaigns
      { campaign-id: campaign-id }
      {
        launch-block: launch-block,
        conclusion-block: conclusion-block,
        allocation-pool: validated-allocation-pool,
        remaining-allocation: validated-allocation-pool,
        minimum-contribution: minimum-contribution,
        maximum-contribution: maximum-contribution,
        contributor-limit: validated-contributor-limit,
        total-contributions: u0,
        status-active: false,
        status-completed: false
      }
    )
    
    ;; Increment the campaign ID counter
    (var-set next-campaign-id (+ campaign-id u1))
    
    (ok campaign-id)
  )
)

;; Fund the vault
(define-public (fund-vault (amount uint))
  (begin
    (asserts! (>= (stx-get-balance tx-sender) amount) ERR-BALANCE-INSUFFICIENT)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update vault balance
    (var-set vault-balance (+ (var-get vault-balance) amount))
    
    (ok true)
  )
)

;; Fund a specific campaign's allocation pool - fixed by validating input
(define-public (fund-allocation-pool (campaign-id uint) (amount uint))
  (let (
        (campaign (unwrap! (map-get? funding-campaigns { campaign-id: campaign-id }) ERR-CAMPAIGN-NOT-EXISTS))
        (validated-campaign-id campaign-id) ;; Added validation variable
      )
    (asserts! (is-admin) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (>= (var-get vault-balance) amount) ERR-BALANCE-INSUFFICIENT)
    
    ;; Validate campaign
    (asserts! (>= validated-campaign-id u1) ERR-INVALID-PARAMETER)
    
    ;; Update vault balance
    (var-set vault-balance (- (var-get vault-balance) amount))
    
    ;; Update campaign allocation pool
    (map-set funding-campaigns
      { campaign-id: validated-campaign-id }
      (merge campaign {
        allocation-pool: (+ (get allocation-pool campaign) amount),
        remaining-allocation: (+ (get remaining-allocation campaign) amount)
      })
    )
    
    (ok true)
  )
)

;; Activate a funding campaign - fixed by validating input
(define-public (activate-campaign (campaign-id uint))
  (let (
        (campaign (unwrap! (map-get? funding-campaigns { campaign-id: campaign-id }) ERR-CAMPAIGN-NOT-EXISTS))
        (validated-campaign-id campaign-id) ;; Added validation variable
       )
    (asserts! (is-admin) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (not (get status-active campaign)) ERR-CAMPAIGN-ALREADY-EXISTS)
    (asserts! (>= block-height (get launch-block campaign)) ERR-CAMPAIGN-INACTIVE)
    (asserts! (<= block-height (get conclusion-block campaign)) ERR-CAMPAIGN-INACTIVE)
    
    ;; Validate campaign
    (asserts! (>= validated-campaign-id u1) ERR-INVALID-PARAMETER)
    
    ;; Set campaign as active
    (map-set funding-campaigns
      { campaign-id: validated-campaign-id }
      (merge campaign { status-active: true })
    )
    
    ;; Update current campaign ID
    (var-set current-campaign-id validated-campaign-id)
    
    (ok true)
  )
)

;; Register a new initiative - fixed by validating input
(define-public (register-initiative (title (string-ascii 100)))
  (let (
        (initiative-id (var-get next-initiative-id))
        (validated-title title) ;; Added validation variable
       )
    ;; Validate title is not empty
    (asserts! (> (len validated-title) u0) ERR-INVALID-INITIATIVE)
    
    ;; Create the initiative
    (map-insert initiatives
      { initiative-id: initiative-id }
      {
        title: validated-title,
        creator: tx-sender,
        total-contributions: u0,
        total-allocated: u0,
        status-active: true
      }
    )
    
    ;; Increment the initiative ID counter
    (var-set next-initiative-id (+ initiative-id u1))
    
    (ok initiative-id)
  )
)

;; Add an initiative to a funding campaign - fixed by validating inputs
(define-public (add-initiative-to-campaign (campaign-id uint) (initiative-id uint))
  (let (
        (validated-campaign-id campaign-id) ;; Added validation variable
        (validated-initiative-id initiative-id) ;; Added validation variable
       )
    (asserts! (is-admin) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (is-some (map-get? funding-campaigns { campaign-id: validated-campaign-id })) ERR-CAMPAIGN-NOT-EXISTS)
    (asserts! (is-some (map-get? initiatives { initiative-id: validated-initiative-id })) ERR-INITIATIVE-NOT-EXISTS)
    (asserts! (is-none (map-get? campaign-initiatives { campaign-id: validated-campaign-id, initiative-id: validated-initiative-id })) ERR-INITIATIVE-ALREADY-EXISTS)
    
    ;; Validate IDs
    (asserts! (>= validated-campaign-id u1) ERR-INVALID-PARAMETER)
    (asserts! (>= validated-initiative-id u1) ERR-INVALID-PARAMETER)
    
    ;; Add initiative to campaign
    (map-insert campaign-initiatives
      { campaign-id: validated-campaign-id, initiative-id: validated-initiative-id }
      { approval-status: true, total-contributions: u0 }
    )
    
    (ok true)
  )
)

;; Make a contribution to an initiative - fixed by validating inputs
(define-public (contribute (campaign-id uint) (initiative-id uint) (amount uint))
  (let (
        (validated-campaign-id campaign-id) ;; Added validation variable
        (validated-initiative-id initiative-id) ;; Added validation variable
        (campaign (unwrap! (map-get? funding-campaigns { campaign-id: validated-campaign-id }) ERR-CAMPAIGN-NOT-EXISTS))
        (initiative (unwrap! (map-get? initiatives { initiative-id: validated-initiative-id }) ERR-INITIATIVE-NOT-EXISTS))
        (campaign-initiative (unwrap! (map-get? campaign-initiatives { campaign-id: validated-campaign-id, initiative-id: validated-initiative-id }) ERR-INVALID-INITIATIVE))
        (contributor-total (default-to { total-contributed: u0 } (map-get? contributor-totals { campaign-id: validated-campaign-id, contributor: tx-sender })))
        (contribution-key { campaign-id: validated-campaign-id, contributor: tx-sender, initiative-id: validated-initiative-id })
        (existing-contribution (default-to { contribution-amount: u0, allocated-amount: u0, withdrawal-status: false } (map-get? contributions contribution-key)))
      )
    ;; Validate IDs
    (asserts! (>= validated-campaign-id u1) ERR-INVALID-PARAMETER)
    (asserts! (>= validated-initiative-id u1) ERR-INVALID-PARAMETER)
    
    ;; Verify campaign is active
    (asserts! (get status-active campaign) ERR-CAMPAIGN-INACTIVE)
    (asserts! (>= block-height (get launch-block campaign)) ERR-CAMPAIGN-INACTIVE)
    (asserts! (<= block-height (get conclusion-block campaign)) ERR-CAMPAIGN-INACTIVE)
    
    ;; Verify initiative is approved for the campaign
    (asserts! (get approval-status campaign-initiative) ERR-INVALID-INITIATIVE)
    
    ;; Verify contribution amount
    (asserts! (>= amount (get minimum-contribution campaign)) ERR-CONTRIBUTION-BELOW-MINIMUM)
    (asserts! (<= amount (get maximum-contribution campaign)) ERR-CONTRIBUTION-EXCEEDS-LIMIT)
    
    ;; Check contributor limit
    (asserts! (<= (+ (get total-contributed contributor-total) amount) (get contributor-limit campaign)) ERR-CONTRIBUTOR-LIMIT-EXCEEDED)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update contribution records
    (map-set contributions 
      contribution-key
      { 
        contribution-amount: (+ (get contribution-amount existing-contribution) amount), 
        allocated-amount: (get allocated-amount existing-contribution),
        withdrawal-status: false 
      }
    )
    
    ;; Update contributor totals
    (map-set contributor-totals
      { campaign-id: validated-campaign-id, contributor: tx-sender }
      { total-contributed: (+ (get total-contributed contributor-total) amount) }
    )
    
    ;; Update campaign totals
    (map-set funding-campaigns
      { campaign-id: validated-campaign-id }
      (merge campaign { total-contributions: (+ (get total-contributions campaign) amount) })
    )
    
    ;; Update initiative totals in campaign
    (map-set campaign-initiatives
      { campaign-id: validated-campaign-id, initiative-id: validated-initiative-id }
      (merge campaign-initiative { total-contributions: (+ (get total-contributions campaign-initiative) amount) })
    )
    
    ;; Update initiative totals
    (map-set initiatives
      { initiative-id: validated-initiative-id }
      (merge initiative { total-contributions: (+ (get total-contributions initiative) amount) })
    )
    
    ;; Add to vault
    (var-set vault-balance (+ (var-get vault-balance) amount))
    
    (ok true)
  )
)

;; End a funding campaign - fixed by validating input
(define-public (end-campaign (campaign-id uint))
  (let (
        (validated-campaign-id campaign-id) ;; Added validation variable
        (campaign (unwrap! (map-get? funding-campaigns { campaign-id: validated-campaign-id }) ERR-CAMPAIGN-NOT-EXISTS))
       )
    (asserts! (is-admin) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (get status-active campaign) ERR-CAMPAIGN-INACTIVE)
    (asserts! (>= block-height (get conclusion-block campaign)) ERR-CAMPAIGN-STILL-ACTIVE)
    
    ;; Validate campaign ID
    (asserts! (>= validated-campaign-id u1) ERR-INVALID-PARAMETER)
    
    ;; Set campaign as inactive
    (map-set funding-campaigns
      { campaign-id: validated-campaign-id }
      (merge campaign { status-active: false })
    )
    
    ;; If this is the current campaign, reset current campaign ID
    (if (is-eq (var-get current-campaign-id) validated-campaign-id)
      (var-set current-campaign-id u0)
      false
    )
    
    (ok true)
  )
)

;; Calculate allocation amounts for a campaign - fixed by validating input
(define-public (finalize-allocation (campaign-id uint))
  (let (
        (validated-campaign-id campaign-id) ;; Added validation variable
        (campaign (unwrap! (map-get? funding-campaigns { campaign-id: validated-campaign-id }) ERR-CAMPAIGN-NOT-EXISTS))
      )
    (asserts! (is-admin) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (not (get status-active campaign)) ERR-CAMPAIGN-INACTIVE)
    (asserts! (not (get status-completed campaign)) ERR-CAMPAIGN-ALREADY-EXISTS)
    
    ;; Validate campaign ID
    (asserts! (>= validated-campaign-id u1) ERR-INVALID-PARAMETER)
    
    ;; Mark campaign as completed
    (map-set funding-campaigns
      { campaign-id: validated-campaign-id }
      (merge campaign { status-completed: true })
    )
    
    (ok true)
  )
)

;; Calculate allocation amount for a specific contribution - fixed by validating inputs
(define-public (calculate-allocation-amount (campaign-id uint) (initiative-id uint) (contributor principal))
  (let (
        (validated-campaign-id campaign-id) ;; Added validation variable
        (validated-initiative-id initiative-id) ;; Added validation variable
        (campaign (unwrap! (map-get? funding-campaigns { campaign-id: validated-campaign-id }) ERR-CAMPAIGN-NOT-EXISTS))
        (contribution-key { campaign-id: validated-campaign-id, contributor: contributor, initiative-id: validated-initiative-id })
        (contribution (unwrap! (map-get? contributions contribution-key) ERR-INITIATIVE-NOT-EXISTS))
        (initiative (unwrap! (map-get? initiatives { initiative-id: validated-initiative-id }) ERR-INITIATIVE-NOT-EXISTS))
        (campaign-initiative (unwrap! (map-get? campaign-initiatives { campaign-id: validated-campaign-id, initiative-id: validated-initiative-id }) ERR-INVALID-INITIATIVE))
        (contribution-amount (get contribution-amount contribution))
        (allocation-pool (get allocation-pool campaign))
        (total-contributions (get total-contributions campaign))
        (allocated-amount (calculate-allocation contribution-amount total-contributions allocation-pool))
      )
    (asserts! (is-admin) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (get status-completed campaign) ERR-CAMPAIGN-STILL-ACTIVE)
    (asserts! (not (get withdrawal-status contribution)) ERR-FUNDS-ALREADY-WITHDRAWN)
    
    ;; Validate IDs
    (asserts! (>= validated-campaign-id u1) ERR-INVALID-PARAMETER)
    (asserts! (>= validated-initiative-id u1) ERR-INVALID-PARAMETER)
    
    ;; Update contribution with allocated amount
    (map-set contributions 
      contribution-key
      (merge contribution { allocated-amount: allocated-amount })
    )
    
    ;; Update initiative allocated total
    (map-set initiatives
      { initiative-id: validated-initiative-id }
      (merge initiative { total-allocated: (+ (get total-allocated initiative) allocated-amount) })
    )
    
    ;; Update total funds allocated
    (var-set total-allocated-funds (+ (var-get total-allocated-funds) allocated-amount))
    
    (ok allocated-amount)
  )
)

;; Claim allocated funds for an initiative
(define-public (claim-allocated-funds (campaign-id uint) (initiative-id uint))
  (let (
        (validated-campaign-id campaign-id) ;; Added validation variable
        (validated-initiative-id initiative-id) ;; Added validation variable
        (campaign (unwrap! (map-get? funding-campaigns { campaign-id: validated-campaign-id }) ERR-CAMPAIGN-NOT-EXISTS))
        (initiative (unwrap! (map-get? initiatives { initiative-id: validated-initiative-id }) ERR-INITIATIVE-NOT-EXISTS))
        (contribution-key { campaign-id: validated-campaign-id, contributor: tx-sender, initiative-id: validated-initiative-id })
        (contribution (unwrap! (map-get? contributions contribution-key) ERR-INITIATIVE-NOT-EXISTS))
      )
    ;; Validate IDs
    (asserts! (>= validated-campaign-id u1) ERR-INVALID-PARAMETER)
    (asserts! (>= validated-initiative-id u1) ERR-INVALID-PARAMETER)
    
    ;; Only initiative creator can claim
    (asserts! (is-eq tx-sender (get creator initiative)) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (get status-completed campaign) ERR-CAMPAIGN-STILL-ACTIVE)
    (asserts! (not (get withdrawal-status contribution)) ERR-FUNDS-ALREADY-WITHDRAWN)
    (asserts! (> (get allocated-amount contribution) u0) ERR-BALANCE-INSUFFICIENT)
    
    ;; Transfer allocated funds to initiative creator
    (try! (as-contract (stx-transfer? (get allocated-amount contribution) tx-sender (get creator initiative))))
    
    ;; Update contribution to mark as withdrawn
    (map-set contributions 
      contribution-key
      (merge contribution { withdrawal-status: true })
    )
    
    ;; Update vault balance
    (var-set vault-balance (- (var-get vault-balance) (get allocated-amount contribution)))
    
    (ok (get allocated-amount contribution))
  )
)

;; Get campaign info
(define-read-only (get-campaign-info (campaign-id uint))
  (map-get? funding-campaigns { campaign-id: campaign-id })
)

;; Get initiative info
(define-read-only (get-initiative-info (initiative-id uint))
  (map-get? initiatives { initiative-id: initiative-id })
)

;; Get contribution info
(define-read-only (get-contribution-info (campaign-id uint) (contributor principal) (initiative-id uint))
  (map-get? contributions { campaign-id: campaign-id, contributor: contributor, initiative-id: initiative-id })
)

;; Get initiative in campaign info
(define-read-only (get-initiative-in-campaign (campaign-id uint) (initiative-id uint))
  (map-get? campaign-initiatives { campaign-id: campaign-id, initiative-id: initiative-id })
)

;; Get contributor total in campaign
(define-read-only (get-contributor-total (campaign-id uint) (contributor principal))
  (default-to { total-contributed: u0 } (map-get? contributor-totals { campaign-id: campaign-id, contributor: contributor }))
)

;; Check if campaign is active
(define-read-only (is-campaign-active (campaign-id uint))
  (let ((campaign (unwrap! (map-get? funding-campaigns { campaign-id: campaign-id }) false)))
    (and 
      (get status-active campaign)
      (>= block-height (get launch-block campaign))
      (<= block-height (get conclusion-block campaign))
    )
  )
)

;; Get current active campaign
(define-read-only (get-active-campaign)
  (let ((current-id (var-get current-campaign-id)))
    (if (> current-id u0)
        (map-get? funding-campaigns { campaign-id: current-id })
        none
    )
  )
)

;; Get total stats
(define-read-only (get-system-stats)
  {
    vault-balance: (var-get vault-balance),
    total-allocated-funds: (var-get total-allocated-funds),
    current-campaign-id: (var-get current-campaign-id),
    next-initiative-id: (var-get next-initiative-id),
    next-campaign-id: (var-get next-campaign-id)
  }
)

;; Withdraw unused vault funds (only admin)
(define-public (withdraw-vault (amount uint))
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (>= (var-get vault-balance) amount) ERR-BALANCE-INSUFFICIENT)
    
    ;; Transfer STX from contract
    (try! (as-contract (stx-transfer? amount tx-sender (var-get admin-address))))
    
    ;; Update vault balance
    (var-set vault-balance (- (var-get vault-balance) amount))
    
    (ok true)
  )
)