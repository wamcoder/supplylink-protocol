;; SupplyLink Protocol - Decentralized Supply Chain Logistics
;; A protocol for tokenizing and managing global logistics capacity

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-capacity (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-already-exists (err u105))

;; Data Variables
(define-data-var token-supply uint u0)
(define-data-var route-nft-counter uint u0)
(define-data-var disruption-pool-balance uint u0)

;; Logistics Utility Token (LUT) balances
(define-map lut-balances principal uint)

;; Route Capacity NFTs
(define-map route-capacity-nfts
  uint
  {
    owner: principal,
    route-name: (string-ascii 64),
    capacity: uint,
    available-capacity: uint,
    warehouse-space: uint,
    active: bool
  }
)

;; Capacity Reservations
(define-map capacity-reservations
  { user: principal, route-id: uint }
  {
    reserved-capacity: uint,
    timestamp: uint,
    duration: uint
  }
)

;; Supply Chain Disruption Events
(define-map disruption-events
  uint
  {
    route-id: uint,
    severity: uint,
    timestamp: uint,
    resolved: bool
  }
)

;; Logistics DAO Proposals
(define-map dao-proposals
  uint
  {
    proposer: principal,
    description: (string-ascii 256),
    votes-for: uint,
    votes-against: uint,
    executed: bool,
    proposal-type: (string-ascii 32)
  }
)

;; User voting records
(define-map user-votes
  { user: principal, proposal-id: uint }
  bool
)

;; Read-only functions

(define-read-only (get-lut-balance (account principal))
  (default-to u0 (map-get? lut-balances account))
)

(define-read-only (get-total-supply)
  (ok (var-get token-supply))
)

(define-read-only (get-route-nft (route-id uint))
  (map-get? route-capacity-nfts route-id)
)

(define-read-only (get-capacity-reservation (user principal) (route-id uint))
  (map-get? capacity-reservations { user: user, route-id: route-id })
)

(define-read-only (get-disruption-event (event-id uint))
  (map-get? disruption-events event-id)
)

(define-read-only (get-dao-proposal (proposal-id uint))
  (map-get? dao-proposals proposal-id)
)

(define-read-only (get-disruption-pool-balance)
  (ok (var-get disruption-pool-balance))
)

;; Public functions

;; Mint Logistics Utility Tokens
(define-public (mint-lut (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> amount u0) err-invalid-amount)
    (let
      (
        (current-balance (get-lut-balance recipient))
        (new-balance (+ current-balance amount))
        (new-supply (+ (var-get token-supply) amount))
      )
      (map-set lut-balances recipient new-balance)
      (var-set token-supply new-supply)
      (ok new-balance)
    )
  )
)

;; Transfer LUT tokens
(define-public (transfer-lut (amount uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) err-unauthorized)
    (asserts! (> amount u0) err-invalid-amount)
    (let
      (
        (sender-balance (get-lut-balance sender))
        (recipient-balance (get-lut-balance recipient))
      )
      (asserts! (>= sender-balance amount) err-insufficient-capacity)
      (map-set lut-balances sender (- sender-balance amount))
      (map-set lut-balances recipient (+ recipient-balance amount))
      (ok true)
    )
  )
)

;; Create Route Capacity NFT
(define-public (mint-route-nft 
  (route-name (string-ascii 64))
  (capacity uint)
  (warehouse-space uint))
  (let
    (
      (new-route-id (+ (var-get route-nft-counter) u1))
    )
    (map-set route-capacity-nfts new-route-id {
      owner: tx-sender,
      route-name: route-name,
      capacity: capacity,
      available-capacity: capacity,
      warehouse-space: warehouse-space,
      active: true
    })
    (var-set route-nft-counter new-route-id)
    (ok new-route-id)
  )
)

;; Reserve logistics capacity
(define-public (reserve-capacity 
  (route-id uint)
  (amount uint)
  (duration uint))
  (let
    (
      (route (unwrap! (map-get? route-capacity-nfts route-id) err-not-found))
      (user-balance (get-lut-balance tx-sender))
      (current-available (get available-capacity route))
    )
    (asserts! (get active route) err-not-found)
    (asserts! (>= current-available amount) err-insufficient-capacity)
    (asserts! (>= user-balance amount) err-insufficient-capacity)
    
    ;; Update route capacity
    (map-set route-capacity-nfts route-id
      (merge route { available-capacity: (- current-available amount) })
    )
    
    ;; Record reservation
    (map-set capacity-reservations 
      { user: tx-sender, route-id: route-id }
      {
        reserved-capacity: amount,
        timestamp: block-height,
        duration: duration
      }
    )
    
    (ok true)
  )
)

;; Release reserved capacity
(define-public (release-capacity (route-id uint))
  (let
    (
      (reservation (unwrap! (map-get? capacity-reservations 
        { user: tx-sender, route-id: route-id }) err-not-found))
      (route (unwrap! (map-get? route-capacity-nfts route-id) err-not-found))
      (reserved-amount (get reserved-capacity reservation))
      (current-available (get available-capacity route))
    )
    ;; Return capacity to route
    (map-set route-capacity-nfts route-id
      (merge route { available-capacity: (+ current-available reserved-amount) })
    )
    
    ;; Remove reservation
    (map-delete capacity-reservations { user: tx-sender, route-id: route-id })
    
    (ok true)
  )
)

;; Report supply chain disruption
(define-public (report-disruption 
  (route-id uint)
  (severity uint)
  (event-id uint))
  (begin
    (asserts! (is-some (map-get? route-capacity-nfts route-id)) err-not-found)
    (map-set disruption-events event-id {
      route-id: route-id,
      severity: severity,
      timestamp: block-height,
      resolved: false
    })
    (ok true)
  )
)

;; Contribute to disruption response pool
(define-public (contribute-to-pool (amount uint))
  (let
    (
      (user-balance (get-lut-balance tx-sender))
      (current-pool (var-get disruption-pool-balance))
    )
    (asserts! (>= user-balance amount) err-insufficient-capacity)
    (map-set lut-balances tx-sender (- user-balance amount))
    (var-set disruption-pool-balance (+ current-pool amount))
    (ok true)
  )
)

;; Create DAO proposal
(define-public (create-proposal 
  (proposal-id uint)
  (description (string-ascii 256))
  (proposal-type (string-ascii 32)))
  (begin
    (asserts! (is-none (map-get? dao-proposals proposal-id)) err-already-exists)
    (map-set dao-proposals proposal-id {
      proposer: tx-sender,
      description: description,
      votes-for: u0,
      votes-against: u0,
      executed: false,
      proposal-type: proposal-type
    })
    (ok true)
  )
)

;; Vote on DAO proposal
(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let
    (
      (proposal (unwrap! (map-get? dao-proposals proposal-id) err-not-found))
      (voter-balance (get-lut-balance tx-sender))
      (already-voted (default-to false 
        (map-get? user-votes { user: tx-sender, proposal-id: proposal-id })))
    )
    (asserts! (not already-voted) err-unauthorized)
    (asserts! (> voter-balance u0) err-insufficient-capacity)
    
    ;; Record vote
    (map-set user-votes { user: tx-sender, proposal-id: proposal-id } true)
    
    ;; Update proposal votes (token-weighted)
    (if vote-for
      (map-set dao-proposals proposal-id
        (merge proposal { votes-for: (+ (get votes-for proposal) voter-balance) })
      )
      (map-set dao-proposals proposal-id
        (merge proposal { votes-against: (+ (get votes-against proposal) voter-balance) })
      )
    )
    (ok true)
  )
)

;; Transfer route NFT ownership
(define-public (transfer-route-nft (route-id uint) (new-owner principal))
  (let
    (
      (route (unwrap! (map-get? route-capacity-nfts route-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get owner route)) err-unauthorized)
    (map-set route-capacity-nfts route-id
      (merge route { owner: new-owner })
    )
    (ok true)
  )
)