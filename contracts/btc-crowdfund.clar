(define-map campaigns
  { id: uint }
  {
    creator: principal,
    goal: uint,
    deadline: uint,
    total-raised: uint,
    status: uint ;; 0 = active, 1 = successful, 2 = failed
  }
)

(define-map contributions
  { campaign-id: uint, contributor: principal }
  {
    amount: uint,
    refunded: bool
  }
)

(define-data-var next-campaign-id uint u0)

(define-constant ERR_CAMPAIGN_NOT_FOUND u100)
(define-constant ERR_DEADLINE_PASSED u101)
(define-constant ERR_ALREADY_CONTRIBUTED u102)
(define-constant ERR_NOT_CREATOR u103)
(define-constant ERR_GOAL_NOT_MET u104)
(define-constant ERR_ALREADY_FINALIZED u105)
(define-constant ERR_NOT_ACTIVE u106)
(define-constant ERR_NOT_REFUNDABLE u107)
(define-constant ERR_ALREADY_REFUNDED u108)
(define-constant STATUS_ACTIVE u0)
(define-constant STATUS_SUCCESSFUL u1)
(define-constant STATUS_FAILED u2)

;; Campaign creation
(define-public (create-campaign (goal uint) (deadline uint) (current-block uint))
  (let ((id (var-get next-campaign-id)))
    (begin
      (asserts! (> deadline current-block) (err ERR_DEADLINE_PASSED))
      (let ((local-goal goal))
        (map-insert campaigns { id: id } {
          creator: tx-sender,
          goal: local-goal,
          deadline: deadline,
          total-raised: u0,
          status: STATUS_ACTIVE
        })
      )
      (var-set next-campaign-id (+ id u1))
      (ok id)
    )
  )
)

;; Contribute STX to campaign

;; Contribute STX to campaign




(define-public (contribute (campaign-id uint) (amount uint) (current-block uint))
  (let (
    (campaign (map-get? campaigns { id: campaign-id }))
    (contributed? (map-get? contributions { campaign-id: campaign-id, contributor: tx-sender }))
  )
    (match campaign
      campaign-data
      (if (not (is-none contributed?))
        (err ERR_ALREADY_CONTRIBUTED)
        (if (not (is-eq (get status campaign-data) STATUS_ACTIVE))
          (err ERR_NOT_ACTIVE)
          (if (not (<= current-block (get deadline campaign-data)))
            (err ERR_DEADLINE_PASSED)
            (let ((transfer-result (stx-transfer? amount tx-sender (as-contract tx-sender))))
              (if (is-ok transfer-result)
                (begin
                  (let ((local-campaign-id campaign-id))
                    (map-insert contributions { campaign-id: local-campaign-id, contributor: tx-sender }
                      { amount: amount, refunded: false })
                    (let ((new-total (+ (get total-raised campaign-data) amount)))
                      (map-set campaigns { id: local-campaign-id }
                        (merge campaign-data { total-raised: new-total }))
                    )
                  )
                  (ok true)
                )
                (err ERR_DEADLINE_PASSED)
              )
            )
          )
        )
      )
      (err ERR_CAMPAIGN_NOT_FOUND)
    )
  )
)

;; Finalize campaign (mark as successful or failed)
(define-public (finalize-campaign (campaign-id uint) (current-block uint))
  (let ((campaign (map-get? campaigns { id: campaign-id })))
    (match campaign campaign-data
      (begin
        (asserts! (is-eq (get status campaign-data) STATUS_ACTIVE) (err ERR_ALREADY_FINALIZED))
        (asserts! (> current-block (get deadline campaign-data)) (err ERR_NOT_ACTIVE))

        (if (>= (get total-raised campaign-data) (get goal campaign-data))
            ;; Goal met
            (let ((local-campaign-id campaign-id))
              (map-set campaigns { id: local-campaign-id }
                (merge campaign-data { status: STATUS_SUCCESSFUL })))
            ;; Goal not met
            (let ((local-campaign-id campaign-id))
              (map-set campaigns { id: local-campaign-id }
                (merge campaign-data { status: STATUS_FAILED })))
        )
        (ok true)
      )
      (err ERR_CAMPAIGN_NOT_FOUND)
    )
  )
)

;; Withdraw funds (creator only, if successful)

(define-public (withdraw-funds (campaign-id uint))
  (let ((campaign (map-get? campaigns { id: campaign-id })))
    (match campaign campaign-data
      (if (not (is-eq tx-sender (get creator campaign-data)))
        (err ERR_NOT_CREATOR)
        (if (not (is-eq (get status campaign-data) STATUS_SUCCESSFUL))
          (err ERR_GOAL_NOT_MET)
          (let ((transfer-result (stx-transfer? (get total-raised campaign-data) (as-contract tx-sender) tx-sender)))
            (if (is-ok transfer-result)
              (ok true)
              (err ERR_GOAL_NOT_MET)
            )
          )
        )
      )
      (err ERR_CAMPAIGN_NOT_FOUND)
    )
  )
)

;; Refund contributor if campaign failed

;; Refund contributor if campaign failed

(define-public (claim-refund (campaign-id uint))
  (let (
    (campaign (map-get? campaigns { id: campaign-id }))
    (contribution (map-get? contributions { campaign-id: campaign-id, contributor: tx-sender }))
  )
    (if (and (is-some campaign) (is-some contribution))
      (let (
        (campaign-data (unwrap! campaign (err ERR_CAMPAIGN_NOT_FOUND)))
        (contribution-data (unwrap! contribution (err ERR_CAMPAIGN_NOT_FOUND)))
      )
        (if (not (is-eq (get status campaign-data) STATUS_FAILED))
          (err ERR_NOT_REFUNDABLE)
          (if (not (is-eq (get refunded contribution-data) false))
            (err ERR_ALREADY_REFUNDED)
            (let ((transfer-result (stx-transfer? (get amount contribution-data) (as-contract tx-sender) tx-sender)))
              (if (is-ok transfer-result)
                (begin
                  (let ((local-campaign-id campaign-id))
                    (map-set contributions { campaign-id: local-campaign-id, contributor: tx-sender }
                      (merge contribution-data { refunded: true })))
                  (ok true)
                )
                (err ERR_NOT_REFUNDABLE)
              )
            )
          )
        )
      )
      (err ERR_CAMPAIGN_NOT_FOUND)
    )
  )
)


;; VIEW: Get campaign info
(define-read-only (get-campaign (campaign-id uint))
  (let ((campaign (map-get? campaigns { id: campaign-id })))
    (if (is-some campaign)
      (ok (unwrap! campaign (err ERR_CAMPAIGN_NOT_FOUND)))
      (err ERR_CAMPAIGN_NOT_FOUND)
    )
  )
)


;; VIEW: Get contribution by user
(define-read-only (get-user-contribution (campaign-id uint) (user principal))
  (let ((contribution (map-get? contributions { campaign-id: campaign-id, contributor: user })))
    (if (is-some contribution)
      (ok (unwrap! contribution (err ERR_CAMPAIGN_NOT_FOUND)))
      (err ERR_CAMPAIGN_NOT_FOUND)
    )
  )
)
