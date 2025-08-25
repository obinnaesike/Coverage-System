;; DECENTRALIZED INSURANCE PROTOCOL
;; A comprehensive blockchain-based insurance system that enables:
;; - Dynamic policy creation with customizable coverage and premiums
;; - Automated premium collection and payment tracking
;; - Transparent claims submission and processing workflow
;; - Decentralized policy management with holder autonomy
;; - Real-time balance management and emergency controls
;; - Immutable record-keeping for all insurance transactions

;; CORE CONTRACT CONSTANTS

(define-constant INSURANCE-PROTOCOL-OWNER tx-sender)

;; ERROR CONSTANTS
(define-constant ERR-UNAUTHORIZED-ACCESS (err u100))
(define-constant ERR-POLICY-NOT-FOUND (err u101))
(define-constant ERR-POLICY-EXPIRED (err u102))
(define-constant ERR-POLICY-ALREADY-CANCELLED (err u103))
(define-constant ERR-INSUFFICIENT-PREMIUM-AMOUNT (err u104))
(define-constant ERR-CLAIM-NOT-FOUND (err u105))
(define-constant ERR-CLAIM-ALREADY-PROCESSED (err u106))
(define-constant ERR-INVALID-COVERAGE-AMOUNT (err u107))
(define-constant ERR-INVALID-PREMIUM-AMOUNT (err u108))
(define-constant ERR-POLICY-ALREADY-EXISTS (err u109))
(define-constant ERR-INSUFFICIENT-CONTRACT-FUNDS (err u110))
(define-constant ERR-INVALID-CLAIM-AMOUNT (err u111))
(define-constant ERR-POLICY-INACTIVE (err u112))

;; POLICY STATUS CONSTANTS
(define-constant POLICY-STATUS-ACTIVE u1)
(define-constant POLICY-STATUS-EXPIRED u2)
(define-constant POLICY-STATUS-CANCELLED u3)

;; CLAIM STATUS CONSTANTS
(define-constant CLAIM-STATUS-PENDING u1)
(define-constant CLAIM-STATUS-APPROVED u2)
(define-constant CLAIM-STATUS-REJECTED u3)
(define-constant CLAIM-STATUS-PAID u4)

;; STATE VARIABLES
(define-data-var next-policy-identifier uint u1)
(define-data-var next-claim-identifier uint u1)
(define-data-var total-contract-balance uint u0)
(define-data-var total-policies-created-count uint u0)
(define-data-var total-claims-submitted-count uint u0)

;; DATA STRUCTURES
;; Primary policy registry
(define-map insurance-policy-registry
  { policy-identifier: uint }
  {
    policy-holder-principal: principal,
    maximum-coverage-limit: uint,
    required-monthly-premium: uint,
    policy-activation-block: uint,
    policy-expiration-block: uint,
    current-policy-status: uint,
    total-premiums-paid-amount: uint,
    policy-creation-timestamp: uint
  }
)

;; Claims management registry
(define-map insurance-claim-registry
  { claim-identifier: uint }
  {
    associated-policy-identifier: uint,
    claim-submitter-principal: principal,
    requested-payout-amount: uint,
    claim-description-text: (string-ascii 500),
    current-claim-status: uint,
    claim-submission-timestamp: uint,
    claim-processing-timestamp: (optional uint),
    claim-processor-principal: (optional principal)
  }
)

;; Premium payment tracking registry
(define-map premium-payment-transaction-registry
  { policy-identifier: uint, payment-sequence-number: uint }
  {
    premium-payment-amount: uint,
    payment-block-height: uint,
    payment-sender-principal: principal,
    payment-execution-timestamp: uint
  }
)

;; Payment sequence tracking per policy
(define-map policy-payment-sequence-registry
  { policy-identifier: uint }
  { total-payment-transactions-count: uint }
)

;; Policy holder registry for quick lookups
(define-map policyholder-policy-mapping
  { holder-principal-address: principal }
  { associated-policy-identifiers: (list 100 uint) }
)

;; UTILITY AND VALIDATION FUNCTIONS
;; Get current blockchain height
(define-read-only (get-current-blockchain-height)
  stacks-block-height
)

;; Verify contract owner privileges
(define-private (verify-contract-owner-authorization (caller-principal principal))
  (is-eq caller-principal INSURANCE-PROTOCOL-OWNER)
)

;; Comprehensive policy validation
(define-private (validate-policy-existence-and-active-status (target-policy-identifier uint))
  (match (map-get? insurance-policy-registry { policy-identifier: target-policy-identifier })
    policy-record-data 
      (if (and 
            (is-eq (get current-policy-status policy-record-data) POLICY-STATUS-ACTIVE)
            (< stacks-block-height (get policy-expiration-block policy-record-data)))
        (ok policy-record-data)
        (if (>= stacks-block-height (get policy-expiration-block policy-record-data))
          ERR-POLICY-EXPIRED
          ERR-POLICY-INACTIVE))
    ERR-POLICY-NOT-FOUND
  )
)

;; Validate claim ownership and status
(define-private (validate-claim-access-authorization (target-claim-identifier uint) (caller-principal principal))
  (match (map-get? insurance-claim-registry { claim-identifier: target-claim-identifier })
    claim-record-data
      (if (is-eq (get claim-submitter-principal claim-record-data) caller-principal)
        (ok claim-record-data)
        ERR-UNAUTHORIZED-ACCESS)
    ERR-CLAIM-NOT-FOUND
  )
)

;; POLICY MANAGEMENT FUNCTIONS
;; Create new insurance policy with enhanced validation
(define-public (create-new-insurance-policy 
  (desired-coverage-limit uint) 
  (monthly-premium-cost uint) 
  (policy-duration-blocks uint))
  (let (
    (new-policy-identifier (var-get next-policy-identifier))
    (policy-activation-block stacks-block-height)
    (policy-expiration-block (+ stacks-block-height policy-duration-blocks))
    (policy-holder-principal tx-sender)
  )
    ;; Input validation
    (asserts! (> desired-coverage-limit u0) ERR-INVALID-COVERAGE-AMOUNT)
    (asserts! (> monthly-premium-cost u0) ERR-INVALID-PREMIUM-AMOUNT)
    (asserts! (> policy-duration-blocks u0) ERR-INVALID-COVERAGE-AMOUNT)
    
    ;; Ensure policy doesn't already exist
    (asserts! (is-none (map-get? insurance-policy-registry { policy-identifier: new-policy-identifier })) 
              ERR-POLICY-ALREADY-EXISTS)
    
    ;; Create comprehensive policy record
    (map-set insurance-policy-registry
      { policy-identifier: new-policy-identifier }
      {
        policy-holder-principal: policy-holder-principal,
        maximum-coverage-limit: desired-coverage-limit,
        required-monthly-premium: monthly-premium-cost,
        policy-activation-block: policy-activation-block,
        policy-expiration-block: policy-expiration-block,
        current-policy-status: POLICY-STATUS-ACTIVE,
        total-premiums-paid-amount: u0,
        policy-creation-timestamp: stacks-block-height
      }
    )
    
    ;; Initialize payment tracking
    (map-set policy-payment-sequence-registry
      { policy-identifier: new-policy-identifier }
      { total-payment-transactions-count: u0 }
    )
    
    ;; Update global counters
    (var-set next-policy-identifier (+ new-policy-identifier u1))
    (var-set total-policies-created-count (+ (var-get total-policies-created-count) u1))
    
    (ok {
      created-policy-identifier: new-policy-identifier,
      coverage-limit-amount: desired-coverage-limit,
      monthly-premium-amount: monthly-premium-cost,
      policy-expiration-block: policy-expiration-block
    })
  )
)

;; Process premium payment with detailed tracking
(define-public (process-premium-payment (target-policy-identifier uint) (premium-payment-amount uint))
  (let (
    ;; Input validation
    (validated-policy-identifier (if (> target-policy-identifier u0) target-policy-identifier u0))
    (validated-payment-amount (if (> premium-payment-amount u0) premium-payment-amount u0))
    (policy-validation-result (validate-policy-existence-and-active-status validated-policy-identifier))
    (current-payment-sequence-data (default-to { total-payment-transactions-count: u0 } 
      (map-get? policy-payment-sequence-registry { policy-identifier: validated-policy-identifier })))
    (next-payment-sequence-number (+ (get total-payment-transactions-count current-payment-sequence-data) u1))
  )
    ;; Additional input validation
    (asserts! (> target-policy-identifier u0) ERR-POLICY-NOT-FOUND)
    (asserts! (> premium-payment-amount u0) ERR-INVALID-PREMIUM-AMOUNT)
    (match policy-validation-result
      policy-record-data (begin
        ;; Validate payment amount meets minimum premium
        (asserts! (>= validated-payment-amount (get required-monthly-premium policy-record-data)) 
                  ERR-INSUFFICIENT-PREMIUM-AMOUNT)
        
        ;; Execute STX transfer to contract
        (try! (stx-transfer? validated-payment-amount tx-sender (as-contract tx-sender)))
        
        ;; Record detailed payment transaction
        (map-set premium-payment-transaction-registry
          { policy-identifier: validated-policy-identifier, payment-sequence-number: next-payment-sequence-number }
          {
            premium-payment-amount: validated-payment-amount,
            payment-block-height: stacks-block-height,
            payment-sender-principal: tx-sender,
            payment-execution-timestamp: stacks-block-height
          }
        )
        
        ;; Update payment sequence counter
        (map-set policy-payment-sequence-registry
          { policy-identifier: validated-policy-identifier }
          { total-payment-transactions-count: next-payment-sequence-number }
        )
        
        ;; Update policy with new total paid amount
        (map-set insurance-policy-registry
          { policy-identifier: validated-policy-identifier }
          (merge policy-record-data { 
            total-premiums-paid-amount: (+ (get total-premiums-paid-amount policy-record-data) validated-payment-amount) 
          })
        )
        
        ;; Update contract balance
        (var-set total-contract-balance (+ (var-get total-contract-balance) validated-payment-amount))
        
        (ok {
          payment-transaction-identifier: next-payment-sequence-number,
          processed-payment-amount: validated-payment-amount,
          updated-total-premiums-paid: (+ (get total-premiums-paid-amount policy-record-data) validated-payment-amount)
        })
      )
      validation-error-result (err validation-error-result)
    )
  )
)

;; Cancel policy with proper authorization
(define-public (cancel-insurance-policy (target-policy-identifier uint))
  (let (
    ;; Input validation
    (validated-policy-identifier (if (> target-policy-identifier u0) target-policy-identifier u0))
    (policy-record-option (map-get? insurance-policy-registry { policy-identifier: validated-policy-identifier }))
  )
    ;; Additional input validation
    (asserts! (> target-policy-identifier u0) ERR-POLICY-NOT-FOUND)
    
    (match policy-record-option
      policy-record-data (begin
        ;; Verify caller is policy holder
        (asserts! (is-eq tx-sender (get policy-holder-principal policy-record-data)) 
                  ERR-UNAUTHORIZED-ACCESS)
        ;; Verify policy is not already cancelled
        (asserts! (not (is-eq (get current-policy-status policy-record-data) POLICY-STATUS-CANCELLED)) 
                  ERR-POLICY-ALREADY-CANCELLED)
        
        ;; Update policy status to cancelled
        (map-set insurance-policy-registry
          { policy-identifier: validated-policy-identifier }
          (merge policy-record-data { current-policy-status: POLICY-STATUS-CANCELLED })
        )
        
        (ok { 
          cancelled-policy-identifier: validated-policy-identifier,
          cancellation-status: "policy-successfully-cancelled",
          cancellation-execution-block: stacks-block-height
        })
      )
      ERR-POLICY-NOT-FOUND
    )
  )
)

;; CLAIMS MANAGEMENT FUNCTIONS
;; Submit insurance claim with comprehensive validation
(define-public (submit-insurance-claim 
  (target-policy-identifier uint) 
  (requested-payout-amount uint) 
  (claim-description-details (string-ascii 500)))
  (let (
    ;; Input validation
    (validated-policy-identifier (if (> target-policy-identifier u0) target-policy-identifier u0))
    (validated-payout-amount (if (> requested-payout-amount u0) requested-payout-amount u0))
    (validated-claim-description (if (> (len claim-description-details) u0) claim-description-details ""))
    (policy-validation-result (validate-policy-existence-and-active-status validated-policy-identifier))
    (new-claim-identifier (var-get next-claim-identifier))
  )
    ;; Additional input validation
    (asserts! (> target-policy-identifier u0) ERR-POLICY-NOT-FOUND)
    (asserts! (> requested-payout-amount u0) ERR-INVALID-CLAIM-AMOUNT)
    (asserts! (> (len claim-description-details) u0) ERR-INVALID-CLAIM-AMOUNT)
    
    (match policy-validation-result
      policy-record-data (begin
        ;; Verify claim submitter is policy holder
        (asserts! (is-eq tx-sender (get policy-holder-principal policy-record-data)) 
                  ERR-UNAUTHORIZED-ACCESS)
        ;; Verify claim amount doesn't exceed coverage
        (asserts! (<= validated-payout-amount (get maximum-coverage-limit policy-record-data)) 
                  ERR-INVALID-CLAIM-AMOUNT)
        
        ;; Create comprehensive claim record
        (map-set insurance-claim-registry
          { claim-identifier: new-claim-identifier }
          {
            associated-policy-identifier: validated-policy-identifier,
            claim-submitter-principal: tx-sender,
            requested-payout-amount: validated-payout-amount,
            claim-description-text: validated-claim-description,
            current-claim-status: CLAIM-STATUS-PENDING,
            claim-submission-timestamp: stacks-block-height,
            claim-processing-timestamp: none,
            claim-processor-principal: none
          }
        )
        
        ;; Update global counters
        (var-set next-claim-identifier (+ new-claim-identifier u1))
        (var-set total-claims-submitted-count (+ (var-get total-claims-submitted-count) u1))
        
        (ok {
          submitted-claim-identifier: new-claim-identifier,
          associated-policy-identifier: validated-policy-identifier,
          requested-payout-amount: validated-payout-amount,
          submission-execution-block: stacks-block-height
        })
      )
      validation-error-result (err validation-error-result)
    )
  )
)

;; Process claim with enhanced tracking and validation
(define-public (process-insurance-claim (target-claim-identifier uint) (claim-approval-decision bool))
  (let (
    ;; Input validation
    (validated-claim-identifier (if (> target-claim-identifier u0) target-claim-identifier u0))
    (claim-record-option (map-get? insurance-claim-registry { claim-identifier: validated-claim-identifier }))
  )
    ;; Additional input validation
    (asserts! (> target-claim-identifier u0) ERR-CLAIM-NOT-FOUND)
    ;; Verify contract owner authorization
    (asserts! (verify-contract-owner-authorization tx-sender) ERR-UNAUTHORIZED-ACCESS)
    
    (match claim-record-option
      claim-record-data (begin
        ;; Verify claim is still pending
        (asserts! (is-eq (get current-claim-status claim-record-data) CLAIM-STATUS-PENDING) 
                  ERR-CLAIM-ALREADY-PROCESSED)
        
        (if claim-approval-decision
          ;; APPROVAL PROCESS
          (begin
            ;; Verify sufficient contract funds
            (asserts! (>= (var-get total-contract-balance) (get requested-payout-amount claim-record-data)) 
                      ERR-INSUFFICIENT-CONTRACT-FUNDS)
            
            ;; Execute payout transfer
            (try! (as-contract (stx-transfer? 
              (get requested-payout-amount claim-record-data) 
              tx-sender 
              (get claim-submitter-principal claim-record-data))))
            
            ;; Update contract balance
            (var-set total-contract-balance 
              (- (var-get total-contract-balance) (get requested-payout-amount claim-record-data)))
            
            ;; Update claim status to paid
            (map-set insurance-claim-registry
              { claim-identifier: validated-claim-identifier }
              (merge claim-record-data {
                current-claim-status: CLAIM-STATUS-PAID,
                claim-processing-timestamp: (some stacks-block-height),
                claim-processor-principal: (some tx-sender)
              })
            )
            
            (ok {
              processed-claim-identifier: validated-claim-identifier,
              claim-processing-status: "claim-approved-and-paid",
              executed-payout-amount: (get requested-payout-amount claim-record-data),
              processing-execution-block: stacks-block-height
            })
          )
          ;; REJECTION PROCESS
          (begin
            (map-set insurance-claim-registry
              { claim-identifier: validated-claim-identifier }
              (merge claim-record-data {
                current-claim-status: CLAIM-STATUS-REJECTED,
                claim-processing-timestamp: (some stacks-block-height),
                claim-processor-principal: (some tx-sender)
              })
            )
            
            (ok {
              processed-claim-identifier: validated-claim-identifier,
              claim-processing-status: "claim-rejected",
              executed-payout-amount: u0,
              processing-execution-block: stacks-block-height
            })
          )
        )
      )
      ERR-CLAIM-NOT-FOUND
    )
  )
)

;; ADMINISTRATIVE FUNCTIONS
;; Emergency fund withdrawal with enhanced security
(define-public (execute-emergency-fund-withdrawal (withdrawal-amount-requested uint))
  (begin
    ;; Verify contract owner authorization
    (asserts! (verify-contract-owner-authorization tx-sender) ERR-UNAUTHORIZED-ACCESS)
    ;; Verify sufficient funds available
    (asserts! (<= withdrawal-amount-requested (var-get total-contract-balance)) 
              ERR-INSUFFICIENT-CONTRACT-FUNDS)
    
    ;; Execute withdrawal transfer
    (try! (as-contract (stx-transfer? withdrawal-amount-requested tx-sender INSURANCE-PROTOCOL-OWNER)))
    
    ;; Update contract balance
    (var-set total-contract-balance (- (var-get total-contract-balance) withdrawal-amount-requested))
    
    (ok {
      executed-withdrawal-amount: withdrawal-amount-requested,
      remaining-contract-balance: (var-get total-contract-balance),
      withdrawal-execution-block: stacks-block-height
    })
  )
)

;; READ-ONLY QUERY FUNCTIONS
;; Retrieve comprehensive policy information
(define-read-only (get-insurance-policy-details (policy-identifier uint))
  (map-get? insurance-policy-registry { policy-identifier: policy-identifier })
)

;; Retrieve detailed claim information
(define-read-only (get-insurance-claim-details (claim-identifier uint))
  (map-get? insurance-claim-registry { claim-identifier: claim-identifier })
)

;; Get current contract financial status
(define-read-only (get-contract-financial-overview)
  {
    total-contract-balance: (var-get total-contract-balance),
    total-policies-created: (var-get total-policies-created-count),
    total-claims-submitted: (var-get total-claims-submitted-count),
    next-policy-identifier: (var-get next-policy-identifier),
    next-claim-identifier: (var-get next-claim-identifier)
  }
)

;; Retrieve specific premium payment details
(define-read-only (get-premium-payment-transaction-details (policy-identifier uint) (payment-sequence-number uint))
  (map-get? premium-payment-transaction-registry { 
    policy-identifier: policy-identifier, 
    payment-sequence-number: payment-sequence-number 
  })
)

;; Get total payment count for a policy
(define-read-only (get-policy-payment-statistics (policy-identifier uint))
  (default-to { total-payment-transactions-count: u0 } 
    (map-get? policy-payment-sequence-registry { policy-identifier: policy-identifier }))
)

;; Check if policy is currently active and valid
(define-read-only (verify-policy-active-status (policy-identifier uint))
  (match (map-get? insurance-policy-registry { policy-identifier: policy-identifier })
    policy-record-data (and 
      (is-eq (get current-policy-status policy-record-data) POLICY-STATUS-ACTIVE)
      (< stacks-block-height (get policy-expiration-block policy-record-data)))
    false
  )
)

;; Get policy holder address
(define-read-only (get-policy-holder-principal-address (policy-identifier uint))
  (match (map-get? insurance-policy-registry { policy-identifier: policy-identifier })
    policy-record-data (some (get policy-holder-principal policy-record-data))
    none
  )
)

;; Get comprehensive policy summary
(define-read-only (get-comprehensive-policy-summary (policy-identifier uint))
  (match (map-get? insurance-policy-registry { policy-identifier: policy-identifier })
    policy-record-data (some {
      policy-identifier: policy-identifier,
      policy-holder-address: (get policy-holder-principal policy-record-data),
      maximum-coverage-amount: (get maximum-coverage-limit policy-record-data),
      required-monthly-premium: (get required-monthly-premium policy-record-data),
      total-premiums-paid: (get total-premiums-paid-amount policy-record-data),
      current-status: (get current-policy-status policy-record-data),
      is-policy-active: (and 
        (is-eq (get current-policy-status policy-record-data) POLICY-STATUS-ACTIVE)
        (< stacks-block-height (get policy-expiration-block policy-record-data))),
      remaining-policy-blocks: (if (> (get policy-expiration-block policy-record-data) stacks-block-height)
        (- (get policy-expiration-block policy-record-data) stacks-block-height)
        u0)
    })
    none
  )
)