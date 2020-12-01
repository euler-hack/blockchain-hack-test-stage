structure ContractPrivate = 
struct
    fun p_get_campaign_task_by_task_id campaign taskId = 
    let
        fun get_task (x::tail) taskId =
            if (get_task_id x) = taskId then
                Some x
            else if tail = [] then 
                None                
            else
                get_task (tail) taskId;
    in 
        get_task (get_campaign_tasks campaign) taskId
    end;

fun p_get_last_price_change campaign = 
    let 
        fun get_price_change (x::tail) =
            if tail = [] then 
                x
            else
                get_price_change tail;
    in
        if (get_campaign_priceChanges campaign) = [] then 
            None
        else
            Some (get_price_change (get_campaign_priceChanges campaign))
    end;

(*functions for Agreement*)
fun update_agreement_details agreement details = 
    set_agreement_details agreement details;

fun approve_agreement agreement = 
    set_agreement_negotiation agreement NegotiationApproved;

fun reject_agreement agreement = 
    set_agreement_negotiation agreement NegotiationRejected;

(*functions for Campaign*)
fun p_update_Campaign_tasks campaign task taskID = 
    let
        fun change_task (x::tail) task taskID = 
            if get_task_id x = taskID then 
                task::tail 
            else 
                x::(change_task tail task taskID)
    in
        set_campaign_tasks (campaign) (change_task (get_campaign_tasks campaign) task taskID) 
    end;

fun p_add_task_in_Campaign campaign task = 
    set_campaign_tasks campaign ((get_campaign_tasks campaign) @ [task]);

fun waitCustomer_in_Campaign campaign = 
    set_campaign_negotiation campaign WaitingCustomer;

fun waitSupplier_in_Campaign campaign = 
    set_campaign_negotiation campaign WaitingSupplier;

fun reject_tasks_in_Campaign campaign = 
    set_campaign_negotiation campaign NegotiationRejected;

fun approve_tasks_in_Campaign campaign = 
    set_campaign_negotiation campaign NegotiationApproved;

fun p_remove_task_from_Campaign campaign taskID =
    let 
        fun remove_task_by_id (x::tail) taskID = 
            if get_task_id x = taskID then 
                tail 
            else 
                x::(remove_task_by_id tail taskID)
    in
        set_campaign_tasks campaign (remove_task_by_id (get_campaign_tasks campaign) taskID)
    end;

fun p_get_paymentOrder_by_id campaign paymentId = 
    let 
        fun get_paymentOrder (x::tail) paymentId = 
            if (get_PaymentOrder_paymentId x) = paymentId then
                Some x 
            else if tail = [] then
                None
            else
                get_paymentOrder (tail) paymentId;
    in
        get_paymentOrder (get_campaign_paymentOrders campaign) paymentId
    end;

fun p_update_paymentOrder_by_id campaign paymentOrder paymentId = 
    let
        fun change_paymentOrder (x::tail) paymentOrder paymentId = 
            if (get_PaymentOrder_paymentId x) = paymentId then 
                paymentOrder::tail 
            else 
                x::(change_paymentOrder tail paymentOrder paymentId);
    in
        set_campaign_paymentOrders campaign (change_paymentOrder (get_campaign_paymentOrders campaign) paymentOrder paymentId)
    end;

fun change_price_campaign campaign priceChange = 
    set_campaign_priceChanges campaign ((get_campaign_priceChanges campaign) @ [priceChange])

fun approve_price_campaign campaign = 
    let 
        fun approve_last_priceChange (x::tail) = 
            if tail = [] then 
                [set_priceChange_negotiation x NegotiationApproved]
            else 
                x::(approve_last_priceChange tail)
    in
        set_campaign_priceChanges campaign ( approve_last_priceChange (get_campaign_priceChanges campaign))
    end;

fun p_perform campaign taskId neededGas = 
    let
        fun change_task (x::tail) taskId = 
            if get_task_id x = taskId then 
                (set_task_requestTime (set_task_requestedGas x neededGas) 0)::tail 
            else 
                x::(change_task tail taskId)
    in
        set_campaign_tasks campaign (change_task (get_campaign_tasks campaign) taskId) 
    end;

fun p_completed campaign taskId suppliedGas = 
    let
        fun change_task (x::tail) taskId = 
            if get_task_id x = taskId then 
                (set_task_suppliedTime (set_task_suppliedGas x suppliedGas) 0)::tail 
            else 
                x::(change_task tail taskId)
    in
        set_campaign_tasks campaign (change_task (get_campaign_tasks campaign) taskId) 
    end;

fun p_confirm campaign taskId = 
    let
        fun change_task (x::tail) taskId = 
            if get_task_id x = taskId then 
                (set_task_completionTime x 0)::tail 
            else 
                x::(change_task tail taskId)
    in
        set_campaign_tasks campaign (change_task (get_campaign_tasks campaign) taskId) 
    end;

fun calculateLastPrice campaign = 
    let 
        fun get_last_approved arr = 
            if (get_priceChange_negotiation (List.last arr)) = NegotiationApproved then 
                Some (get_priceChange_price (List.last arr))
            else if List.length arr = 1 then
                None
            else
                get_last_approved (List.take arr ((List.length arr) - 1))
    in 
        get_last_approved (get_campaign_priceChanges campaign)
    end;


(* (Task list) -> int -> Task *)
fun p_get_task_by_id taskList taskId = 
    case taskList of
       [] => NONE ("Task not found")
     | (x::tail) => 
        if get_task_id x = taskId then 
           SOME x 
        else 
            p_get_task_by_id tail taskId;

fun get_element_with_number (x::tail) number = if number = 0 then x else get_element_with_number tail (number-1);
    
(* a' list -> a' *)
fun get_last_element (x::tail) = if tail = [] then x else get_last_element tail;

(* a' list -> a' list *)
fun cut_last_element (x::tail) = if tail = [] then [] else x::(cut_last_element tail);

(* Campaign -> int -> Campaign *)
fun p_approve_task campaign taskId = 
        let
        fun change_task (x::tail) taskId = 
            if get_task_id x = taskId then 
                (set_task_negotiation x NegotiationApproved)::tail 
            else 
                x::(change_task tail taskId)
    in
        set_campaign_tasks campaign (change_task (get_campaign_tasks campaign) taskId) 
    end;

(* Campaign -> int -> Campaign *)
fun p_reject_task campaign taskId = 
        let
        fun change_task (x::tail) taskId = 
            if get_task_id x = taskId then 
                (set_task_negotiation x NegotiationRejected)::tail 
            else 
                x::(change_task tail taskId)
    in
        set_campaign_tasks campaign (change_task (get_campaign_tasks campaign) taskId) 
    end;

(* Campaign -> int -> Campaign *)
fun p_accept_task campaign taskId = 
    let
        fun change_task (x::tail) taskId = 
            if get_task_id x = taskId then 
                (set_task_taskStatus x TaskAccepted)::tail 
            else 
                x::(change_task tail taskId)
    in
        set_campaign_tasks campaign (change_task (get_campaign_tasks campaign) taskId) 
    end;
end;

structure Contract = 
struct

(* Context -> (SCValue list) -> (SCValue OptionErr)*)
fun constructor context params = 
    let
        (* (SCValue list) -> (SCValue OptionErr) *)
        fun validate_params params = 
            let
                val customer_addr = scvalue_to_int ((List.nth params 0));
                val customer_name = scvalue_to_string ((List.nth params 1));
                val supplier_addr = scvalue_to_int ((List.nth params 2));
                val supplier_name = scvalue_to_string ((List.nth params 3));
                val agreement_details = scvalue_to_string ((List.nth params 4));
                val bank_addr = scvalue_to_int ((List.nth params 5));
            in 
                if (((List.length params) = 6) andalso
                ((Option.isSome customer_addr) andalso ((Option.isSome customer_name) andalso
                ((Option.isSome supplier_name) andalso ((Option.isSome supplier_addr) andalso 
                ((Option.isSome agreement_details) andalso (Option.isSome bank_addr))))))) then
                    SOME (SCBool True)
                else
                    NONE "Parse params error."                                                    
            end;

        (* (SCValue list) -> (SCValue OptionErr) *)
        fun create params campaign =
            let
                val customer_addr       = (Option.valOf (scvalue_to_int    ((List.nth params 0))));
                val customer_name       = (Option.valOf (scvalue_to_string ((List.nth params 1))));
                val supplier_addr       = (Option.valOf (scvalue_to_int    ((List.nth params 2))));
                val supplier_name       = (Option.valOf (scvalue_to_string ((List.nth params 3))));
                val agreement_details   = (Option.valOf (scvalue_to_string ((List.nth params 4))));
                val bank_addr           = (Option.valOf (scvalue_to_int    ((List.nth params 5))));
            in 
                if (get_context_msgSender context) = customer_addr then
                    SOME (SCCampaign (Campaign (Agreement WaitingSupplier (Person customer_addr customer_name) (Person supplier_addr supplier_name) (AgreementDetails agreement_details bank_addr)) [] WaitingCustomer [] PhaseAgreement bank_addr []))
                else
                    NONE ( "Only customer allowed to create agreement")
            end;
        val validated = (validate_params params);
    in
        if Option.isNone (get_err validated) then 
            create params context
        else
            validated
    end; 

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun getAgreement context params campaign = 
    let
        val sender = (get_context_msgSender context);
        val customer = get_agreement_customer (get_campaign_agreement campaign);
        val supplier = get_agreement_supplier (get_campaign_agreement campaign)
    in 
        if ((sender = (get_person_addr supplier)) orelse (sender = (get_person_addr customer))) then
            RET (SCCampaign campaign) (SCAgreement (get_campaign_agreement campaign))
        else
            NONE ( "Only customer or supplier allowed to view agreement")
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun rejectAgreement context params campaign = 
    let
        val sender = (get_context_msgSender context);
        val agreement = (get_campaign_agreement campaign);
    in 
        if sender = (get_person_addr( get_agreement_supplier agreement))then
            if (get_campaign_phase campaign) = PhaseAgreement then
                if (get_agreement_negotiation agreement) = WaitingSupplier then
                    SOME (SCCampaign (set_campaign_agreement campaign (set_agreement_negotiation agreement WaitingCustomer)))
                else
                    NONE ( "Supplier is not allowed to reject at this point")
            else 
                NONE ( "Supplier is not allowed to reject at this point")
        else
            NONE ( "Only supplier allowed to reject agreement")
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun approveAgreement context params campaign = 
    let
        val sender = (get_context_msgSender context);
        val agreement = (get_campaign_agreement campaign);
    in 
        if sender = (get_person_addr((get_agreement_supplier agreement)))then
            if (get_campaign_phase campaign) = PhaseAgreement then
                if (get_agreement_negotiation agreement) = WaitingSupplier then 
                    SOME (SCCampaign (set_campaign_agreement (set_campaign_phase campaign PhaseTasks) (set_agreement_negotiation agreement NegotiationApproved)))
                else
                    NONE ( "Supplier is not allowed to approve at this point")
            else 
                NONE ( "Supplier is not allowed to approve at this point")
        else
            NONE ( "Only supplier allowed to approve agreement")
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun changeAgreementDetails context params campaign = 
    let
        val sender = (get_context_msgSender context);
        val agreement = (get_campaign_agreement campaign);
        val details = (Option.valOf (scvalue_to_string (List.nth params 0))); 
        val bankAddr = (Option.valOf (scvalue_to_int (List.nth params 1))); 
    in 
        if sender = (get_person_addr((get_agreement_customer agreement)))then
            if (get_campaign_phase campaign) = PhaseAgreement then
                if (get_agreement_negotiation agreement) = WaitingCustomer then
                    SOME (SCCampaign (set_campaign_agreement campaign (set_agreement_details (set_agreement_negotiation agreement WaitingSupplier) (AgreementDetails details bankAddr))))
                else 
                    NONE ( "Customer is not allowed to change details at this point")
            else 
                NONE ( "Customer is not allowed to change details at this point")
        else
            NONE ( "Only customer allowed to change details")
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun getPriceChangeWithNumber context params campaign = 
    let
    fun validate_params params = 
        let
          val index = (scvalue_to_int (List.nth params 0));
        in
          if (List.length params) = 1 then
            if (Option.isSome index) then
                 SOME (SCBool True)
            else
                NONE "Parse param error: index incorrect type."
          else
            NONE "Wrong number of params"
        end
        
        fun getPriceChange context params campaign = 
            let
        val sender = (get_context_msgSender context);
        val index = Option.valOf(scvalue_to_int (List.nth params 0));
    in 
        if sender = get_person_addr (get_agreement_customer (get_campaign_agreement campaign)) then
            if (List.length (get_campaign_priceChanges campaign)) > 0 then
                RET (SCCampaign campaign) (SCPriceChange (ContractPrivate.get_element_with_number (get_campaign_priceChanges campaign) index)) 
            else 
                NONE ( "No price changes")
        else
            NONE ( "Only customer allowed to view PriceChange")
    end

        val validated = (validate_params params);
    in
        if Option.isNone (get_err validated) then 
            getPriceChange context params campaign
        else
            validated
    end; 

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun getPriceChangesLength context params campaign = 
    let
        val sender = (get_context_msgSender context);
    in 
        if sender = get_person_addr (get_agreement_customer (get_campaign_agreement campaign)) then
            if (List.length (get_campaign_priceChanges campaign)) > 0 then 
                RET (SCCampaign campaign) (SCInt ((List.length (get_campaign_priceChanges campaign)-1)))
            else 
                NONE ( "No price changes")
        else
            NONE ( "Only customer allowed to view count of PriceChange")
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun rejectPrice context params campaign = 
    let
        val sender = (get_context_msgSender context);
    in 
        if sender = get_person_addr (get_agreement_customer (get_campaign_agreement campaign)) then
            if (get_campaign_phase campaign) = PhaseTasks then
                if (List.length (get_campaign_priceChanges campaign)) > 0 then 
                    if (get_priceChange_negotiation (ContractPrivate.get_last_element (get_campaign_priceChanges campaign))) = WaitingCustomer then 
                        SOME (SCCampaign (set_campaign_priceChanges campaign ( (ContractPrivate.cut_last_element (get_campaign_priceChanges campaign)) @ [(set_priceChange_negotiation (ContractPrivate.get_last_element (get_campaign_priceChanges campaign)) NegotiationRejected)] )))
                    else 
                        NONE ( "Customer is not allowed to reject at this point")
                else 
                    NONE ( "No price changes")
            else
                NONE ( "Phase is not PhaseTasks")
        else
            NONE ( "Only customer allowed to reject PriceChange")
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun approvePrice context params campaign = 
    let
        val sender = (get_context_msgSender context);
    in 
        if sender = get_person_addr (get_agreement_customer (get_campaign_agreement campaign)) then
            if (get_campaign_phase campaign) = PhaseTasks then
                if (List.length (get_campaign_priceChanges campaign)) > 0 then 
                    if (get_priceChange_negotiation (ContractPrivate.get_last_element (get_campaign_priceChanges campaign))) = WaitingCustomer then 
                        SOME (SCCampaign (set_campaign_priceChanges campaign ( (ContractPrivate.cut_last_element (get_campaign_priceChanges campaign)) @ [(set_priceChange_negotiation (ContractPrivate.get_last_element (get_campaign_priceChanges campaign)) NegotiationApproved)])))
                    else 
                        NONE ( "Customer is not allowed to approve at this point")
                else 
                    NONE ( "No price changes")
            else
                NONE ( "Phase is not PhaseTasks")
        else
            NONE ( "Only customer allowed to approve PriceChange")
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun declinePrice context params campaign = 
    let
        val sender = (get_context_msgSender context);
    in 
        if sender = get_person_addr (get_agreement_customer (get_campaign_agreement campaign)) then
            if (get_campaign_phase campaign) = PhaseTasks then
                if (List.length (get_campaign_priceChanges campaign)) > 0 then 
                    if (get_priceChange_negotiation (ContractPrivate.get_last_element (get_campaign_priceChanges campaign))) = WaitingCustomer then 
                        SOME (SCCampaign (set_campaign_phase campaign PhaseDeclined))
                    else 
                        NONE ( "Customer is not allowed to decline at this point")
                else 
                    NONE ( "No price changes")
            else
                NONE ( "Phase is not PhaseTasks")
        else
            NONE ( "Only customer allowed to decline PriceChange")
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun createPriceChange context params campaign = 
    let
    fun validate_params params = 
        let
            val price =       (scvalue_to_int( List.nth params 0));
            val negotiation = (scValue_to_negotiation( List.nth params 1));
            val startTime =   (scvalue_to_int( List.nth params 2));
        in
          if (List.length params) = 3 then
            if (Option.isSome price) then
                if (Option.valOf price) > 0 then
                    if (Option.isSome negotiation) then
                        if (Option.isSome startTime) then
                            if (Option.valOf startTime) > 0 then
                                SOME (SCBool True)
                            else
                                NONE "Start Time must be more than 0."
                        else
                            NONE "Parse param error: startTime incorrect type."
                    else
                        NONE "Parse param error: negotiation incorrect type."
                else
                    NONE "Price must be more than 0."
            else
                NONE "Parse param error: price incorrect type."
          else
            NONE "Wrong number of params"
        end
        
    (* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
    fun create context params campaign = 
    let
        val sender = (get_context_msgSender context);

        val price = Option.valOf (scvalue_to_int( List.nth params 0));
        val negotiation = Option.valOf(scValue_to_negotiation( List.nth params 1));
        val startTime = Option.valOf(scvalue_to_int( List.nth params 2));
    in 
        if sender = get_person_addr (get_agreement_supplier (get_campaign_agreement campaign)) then
            if (get_campaign_phase campaign) = PhaseTasks then
                if (List.length (get_campaign_priceChanges campaign)) > 0 then 
                    if (get_priceChange_negotiation (ContractPrivate.get_last_element (get_campaign_priceChanges campaign))) = NegotiationApproved then 
                        SOME (SCCampaign (set_campaign_priceChanges campaign ((get_campaign_priceChanges campaign) @ [ (PriceChange price negotiation startTime )] )))
                    else if (get_priceChange_negotiation (ContractPrivate.get_last_element (get_campaign_priceChanges campaign))) = NegotiationRejected then 
                        SOME (SCCampaign (set_campaign_priceChanges campaign ((get_campaign_priceChanges campaign) @ [ (PriceChange price negotiation startTime )] )))
                    else 
                        NONE ("The last change of price should be approved or rejected before adding another priceChange")
                else 
                    SOME (SCCampaign (set_campaign_priceChanges campaign [(PriceChange price negotiation startTime )] ))
            else
                NONE ("Phase is not PhaseTasks")
        else
            NONE ("Only supplier allowed to create PriceChange")
    end
            
        val validated = (validate_params params);
    in
        if Option.isNone (get_err validated) then 
            create context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun getTask context params campaign = 
    let
    fun validate_params params = 
        let
            val taskId = (scvalue_to_int (List.nth params 0));
        in 
            if (Option.isSome taskId) then 
                if Option.isSome (ContractPrivate.p_get_campaign_task_by_task_id campaign (Option.valOf taskId)) then
                    SOME (SCBool True)
                else
                    NONE "Task does not exist."
            else
                NONE "Parse param error: taskId. Incorrect type."
        end;
        
    (* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
    fun action context params campaign = 
        let
        val sender = (get_context_msgSender context);
        val taskId = Option.valOf (scvalue_to_int (List.hd params));
        val task = (Option.valOf(ContractPrivate.p_get_campaign_task_by_task_id campaign taskId));
        val agreement = (get_campaign_agreement campaign);
    in 
        if ((sender = get_person_addr (get_agreement_supplier agreement )) orelse (sender = get_person_addr (get_agreement_customer agreement))) then
            RET (SCCampaign campaign) (SCTask task)
        else
            NONE ( "Only supplier or customer allowed to get task")
    end

        val validated = (validate_params params);
    in
        if Option.isNone (get_err validated) then 
            action context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun approveTask context params campaign = 
    let
    fun validate_params params = 
        let
          val taskId =  (scvalue_to_int (List.hd params));
        in
          if (List.length params) = 1 then
            if (Option.isSome taskId) then
                if (Option.valOf taskId) > 0 then
                    SOME (SCBool True)
                 else
                    NONE "Price must be more than 0."
            else
                NONE "Parse param error: taskId incorrect type."
          else
            NONE "Wrong number of params"
        end
        
    (* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
    fun approve context params campaign = 
        let
        val sender = (get_context_msgSender context);
        val taskId = Option.valOf (scvalue_to_int (List.hd params));
    in 
        if Option.isNone (get_err (ContractPrivate.p_get_task_by_id (get_campaign_tasks campaign) taskId)) then
            if sender = get_person_addr (get_agreement_supplier (get_campaign_agreement campaign)) then
                if (get_campaign_phase campaign) = PhaseTasks then
                    if get_task_negotiation (Option.valOf (get (ContractPrivate.p_get_task_by_id (get_campaign_tasks campaign) taskId))) <> NegotiationRejected then
                        SOME (SCCampaign (ContractPrivate.p_approve_task campaign taskId))
                    else
                        NONE ( "Task was rejected")
                else
                    NONE ( "Phase is not PhaseTasks")
            else
                NONE ( "Only supplier allowed to approve")
        else
            NONE ( "Task does not exists.")
    end

        val validated = (validate_params params);
    in
        if Option.isNone (get_err validated) then 
            approve context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun rejectTask context params campaign = 
    let
    fun validate_params params = 
        let
          val taskId = scvalue_to_int (List.hd params);
        in
          if (List.length params) = 1 then
            if (Option.isSome taskId) then
                if (Option.valOf taskId) > 0 then
                    SOME (SCBool True)
                 else
                    NONE "Price must be more than 0."
            else
                NONE "Parse param error: taskId incorrect type."
          else
            NONE "Wrong number of params"
        end
        
    (* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
    fun reject context params campaign = 
    let
        val sender = get_context_msgSender context;
        val taskId = Option.valOf (scvalue_to_int (List.hd params));
    in 
        if Option.isNone (get_err (ContractPrivate.p_get_task_by_id (get_campaign_tasks campaign) taskId)) then
            if sender = get_person_addr (get_agreement_supplier (get_campaign_agreement campaign)) then
                if (get_campaign_phase campaign) = PhaseTasks then
                    if get_task_negotiation (Option.valOf (get (ContractPrivate.p_get_task_by_id (get_campaign_tasks campaign) taskId))) <> NegotiationApproved then
                            SOME (SCCampaign (ContractPrivate.p_reject_task campaign taskId))
                    else
                        NONE ( "Task was approved")
                else
                    NONE ( "Phase is not PhaseTasks")
            else
                NONE ( "Only supplier allowed to reject Task")
        else
            NONE ( "Task does not exists.")
    end

        val validated = (validate_params params);
    in
        if Option.isNone (get_err validated) then 
            reject context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun acceptTask context params campaign = 
    let
        fun validate_params params = 
            let
            val taskId = (scvalue_to_int (List.hd params));
            in
            if (List.length params) = 1 then
                if (Option.isSome taskId) then
                    if (Option.valOf taskId) > 0 then
                        SOME (SCBool True)
                    else
                        NONE "Price must be more than 0."
                else
                    NONE "Parse param error: taskId incorrect type."
            else
                NONE "Wrong number of params"
            end;
        
    (* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
    fun accept context params campaign = 
        let
        val sender = (get_context_msgSender context);
        val taskId = Option.valOf(scvalue_to_int (List.hd params));
        in 
            if Option.isNone (get_err (ContractPrivate.p_get_task_by_id (get_campaign_tasks campaign) taskId)) then
                if sender = get_person_addr (get_task_worker (Option.valOf (get (ContractPrivate.p_get_task_by_id (get_campaign_tasks campaign) taskId)))) then
                    if (get_campaign_phase campaign) = PhaseTasks then
                        if get_task_negotiation (Option.valOf (get (ContractPrivate.p_get_task_by_id (get_campaign_tasks campaign) taskId))) = NegotiationApproved then
                            SOME (SCCampaign (ContractPrivate.p_accept_task campaign taskId))
                        else
                            NONE ( "Task is not approved yet.")
                    else
                        NONE ( "Phase is not PhaseTasks")
                else
                    NONE ( "Only worker allowed to accept Task")
            else
                NONE ( "Task does not exists.")
        end;

        val validated = (validate_params params);
    in
        if Option.isNone (get_err validated) then 
            accept context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun removeTask context params campaign = 
    let
    fun validate_params params = 
        let
          val taskId = (scvalue_to_int (List.hd params));
        in
          if (List.length params) = 1 then
            if (Option.isSome taskId) then
                if (Option.valOf taskId) > 0 then
                    SOME (SCBool True)
                 else
                    NONE "Price must be more than 0."
            else
                NONE "Parse param error: taskId incorrect type."
          else
            NONE "Wrong number of params"
        end
        
    (* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
    fun remove context params campaign = 
    let
        val sender = (get_context_msgSender context);
        val taskId = Option.valOf(scvalue_to_int (List.hd params));
    in 
        if Option.isNone (get_err (ContractPrivate.p_get_task_by_id (get_campaign_tasks campaign) taskId)) then
            if sender = get_person_addr (get_agreement_customer (get_campaign_agreement campaign)) then
                if (get_campaign_phase campaign) = PhaseTasks then
                        SOME (SCCampaign (ContractPrivate.p_remove_task_from_Campaign campaign taskId) )
                else
                    NONE ( "Phase is not PhaseTasks")
            else
                NONE ( "Only customer allowed to remove Task")
        else
            NONE ( "Task does not exists.")
    end

        val validated = (validate_params params);
    in
        if Option.isNone (get_err validated) then 
            remove context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun addTask context params campaign = 
     let
        fun validate_params params = 
        let
          val taskId =         (scvalue_to_int (List.nth params 0));
          val negotiation =    (scValue_to_negotiation (List.nth params 1));
          val captain_addr =   (scvalue_to_int (List.nth params 2));
          val captain_name =   (scvalue_to_string (List.nth params 3));
          val worker_addr =    (scvalue_to_int (List.nth params 4));
          val worker_name =    (scvalue_to_string (List.nth params 5));
          val expectedGas =    (scvalue_to_int (List.nth params 6));
          val requestedGas =   (scvalue_to_int (List.nth params 7));
          val suppliedGas =    (scvalue_to_int (List.nth params 8));
          val totalGas =       (scvalue_to_int (List.nth params 9));
          val requestTime =    (scvalue_to_int (List.nth params 10));
          val suppliedTime =   (scvalue_to_int (List.nth params 11));
          val completionTime = (scvalue_to_int (List.nth params 12));
          val paymentTime =    (scvalue_to_int (List.nth params 13));
          val taskStatus =     (scValue_to_taskStatus (List.nth params 14));
          val paymentType =    (scValue_to_paymentType (List.nth params 15));
        in
            if (((List.length params) = 16) andalso
            ((Option.isSome taskId) andalso ((Option.isSome negotiation) andalso
            ((Option.isSome captain_name) andalso ((Option.isSome captain_addr) andalso
            ((Option.isSome worker_name) andalso ((Option.isSome worker_addr) andalso
            ((Option.isSome expectedGas) andalso ((Option.isSome requestedGas) andalso
            ((Option.isSome suppliedGas) andalso ((Option.isSome totalGas) andalso
            ((Option.isSome requestTime) andalso ((Option.isSome suppliedTime) andalso
            ((Option.isSome completionTime) andalso ((Option.isSome paymentTime) andalso
            ((Option.isSome taskStatus) andalso (Option.isSome paymentType))))))))))))))))) then
                SOME (SCBool True)
            else
                NONE "Parse param error"
        end;
        
    (* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
    fun add context params campaign = 
        let
            val sender = (get_context_msgSender context);

          val taskId =         Option.valOf (scvalue_to_int (List.nth params 0));
          val negotiation =    Option.valOf (scValue_to_negotiation (List.nth params 1));
          val captain_addr =   Option.valOf (scvalue_to_int (List.nth params 2));
          val captain_name =   Option.valOf (scvalue_to_string (List.nth params 3));
          val worker_addr =    Option.valOf (scvalue_to_int (List.nth params 4));
          val worker_name =    Option.valOf (scvalue_to_string (List.nth params 5));
          val expectedGas =    Option.valOf (scvalue_to_int (List.nth params 6));
          val requestedGas =   Option.valOf (scvalue_to_int (List.nth params 7));
          val suppliedGas =    Option.valOf (scvalue_to_int (List.nth params 8));
          val totalGas =       Option.valOf (scvalue_to_int (List.nth params 9));
          val requestTime =    Option.valOf (scvalue_to_int (List.nth params 10));
          val suppliedTime =   Option.valOf (scvalue_to_int (List.nth params 11));
          val completionTime = Option.valOf (scvalue_to_int (List.nth params 12));
          val paymentTime =    Option.valOf (scvalue_to_int (List.nth params 13));
          val taskStatus =     Option.valOf (scValue_to_taskStatus (List.nth params 14));
          val paymentType =     Option.valOf (scValue_to_paymentType (List.nth params 15));
        in 
        if sender = get_person_addr( get_agreement_customer (get_campaign_agreement campaign) )then
            if (get_campaign_phase campaign) = PhaseTasks then
                    SOME (SCCampaign (ContractPrivate.p_add_task_in_Campaign campaign (Task taskId negotiation (Person captain_addr captain_name) (Person worker_addr worker_name) expectedGas requestedGas suppliedGas totalGas requestTime suppliedTime completionTime paymentTime taskStatus paymentType) ))
            else
                NONE ( "Phase is not PhaseTasks")
        else
            NONE ( "Only customer allowed to add Task")
    end;

        val validated = (validate_params params);
    in
        if Option.isNone (get_err validated) then 
            add context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun readyToPerformTask context params campaign = 
    let 
        val sender = (get_context_msgSender context);

        fun validate_params params campaign = 
            let
                val taskId = (scvalue_to_int (List.nth params 0));
            in 
                if Option.isSome taskId then 
                    if Option.isSome(ContractPrivate.p_get_campaign_task_by_task_id campaign (Option.valOf taskId)) then
                        SOME (SCBool True)
                    else
                        NONE ("Task does not exist.")
                else
                    NONE ("Parse param error: TaskID. Incorrect type.")
            end;

        fun action context params campaign =
            let 
                val taskId = (Option.valOf(scvalue_to_int (List.nth params 0)));
                val task = (Option.valOf(ContractPrivate.p_get_campaign_task_by_task_id campaign taskId));
            in 
                if sender = (get_person_addr (get_task_worker task)) then
                    if (get_campaign_phase campaign) = PhaseTasks then
                        if (get_task_taskStatus task) = TaskAccepted then
                            SOME (SCCampaign (ContractPrivate.p_update_Campaign_tasks campaign (set_task_taskStatus task TaskReadyToPerform) taskId))
                        else 
                            NONE ( "Task is not accepted yet.")
                    else
                        NONE ( "Action is not allowed at this point")
                else 
                    NONE ( "Only worker allowed to do this action.")
            end;

        val validated = (validate_params params campaign);
    in
        if Option.isNone (get_err validated) then 
            action context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun requestGas context params campaign = 
    let 
        val sender = (get_context_msgSender context);
        val requestTime = (get_context_blockNum context);

        fun validate_params params campaign = 
            let
                val taskId = (scvalue_to_int (List.nth params 0));
                val amount = (scvalue_to_int (List.nth params 1));
                val paymentTime = (scvalue_to_int (List.nth params 2));

                val priceChange = (ContractPrivate.p_get_last_price_change campaign);
            in 
                if Option.isSome taskId then 
                    if Option.isSome (ContractPrivate.p_get_campaign_task_by_task_id campaign (Option.valOf taskId )) then 
                        if Option.isSome amount then
                            if (Option.valOf amount) > 0 then
                                if Option.isSome paymentTime then 
                                    if (Option.valOf paymentTime) > 0 then
                                        if ( (get_task_paymentType (Option.valOf(ContractPrivate.p_get_campaign_task_by_task_id campaign (Option.valOf taskId )))) = Pre ) andalso (Option.isNone (ContractPrivate.calculateLastPrice campaign)) then
                                            NONE "No approved prices."
                                        else
                                            SOME (SCBool True)
                                    else
                                        NONE "Payment time must be more than 0."
                                else
                                    NONE  "Parse param error: PaymentTime. Incorrect type."
                            else
                                NONE "Amount must be more than 0."
                        else
                            NONE "Parse param error: Amount. Incorrect type."
                    else
                        NONE "Task does not exist."
                else
                    NONE "Parse param error: TaskId. Incorrect type."
            end;

        fun action context params campaign = 
            let
                val taskId = (Option.valOf (scvalue_to_int (List.nth params 0)));
                val amount = (Option.valOf (scvalue_to_int (List.nth params 1)));
                val paymentTime = (Option.valOf (scvalue_to_int (List.nth params 2)));

                val task = (Option.valOf(ContractPrivate.p_get_campaign_task_by_task_id campaign taskId));
                val price = Option.valOf (ContractPrivate.calculateLastPrice campaign);

                (* Task -> Task *)
                fun update_info task = (set_task_requestTime (set_task_requestedGas (set_task_totalGas (set_task_taskStatus task GasRequested) ((get_task_totalGas task) + amount)) amount) requestTime)
                (* Campaign -> Campaign *)
                fun add_payment_order campaign = 
                    if (get_task_paymentType task = Pre) then 
                        set_campaign_paymentOrders campaign (( get_campaign_paymentOrders campaign) @ [(PaymentOrder (amount*price) paymentTime 0 taskId WaitingForPayment True)])
                    else
                        campaign;
                                    
            in
                if sender = (get_person_addr (get_task_captain task)) then
                    if (get_campaign_phase campaign) = PhaseTasks then
                        if (get_task_taskStatus task = TaskReadyToPerform) then
                            SOME (SCCampaign 
                                (ContractPrivate.p_update_Campaign_tasks (add_payment_order campaign) (update_info task) taskId)
                            )
                        else 
                            NONE ("Task is not ready to be performed.")
                    else
                        NONE ("Action is not allowed at this point.")
                else 
                    NONE ("Only captain allowed to do this action.")
            end;
        val validated = (validate_params params campaign);
        
    in
        if Option.isNone (get_err validated) then 
            action context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun paymentCompleted context params campaign = 
    let 
        val sender = (get_context_msgSender context);
        val requestTime = (get_context_blockNum context);

        fun validate_params params campaign = 
            let
                val paymentId = (scvalue_to_int (List.nth params 0));
            in 
                if (Option.isSome paymentId) then 
                    if (Option.isSome (ContractPrivate.p_get_paymentOrder_by_id campaign ( Option.valOf paymentId))) then
                        if Option.isSome (ContractPrivate.p_get_campaign_task_by_task_id campaign (get_PaymentOrder_taskId (Option.valOf (ContractPrivate.p_get_paymentOrder_by_id campaign ( Option.valOf paymentId))))) then
                            SOME (SCBool True)
                        else
                            NONE "Task does not exist."
                    else 
                        NONE "Payment order does not exist."
                else
                    NONE "Parse param error: PaymentId. Incorrect type."
            end;

        fun action context params campaign = 
            let
                val paymentId = (Option.valOf(scvalue_to_int (List.nth params 0)));
                val paymentOrder = (Option.valOf(ContractPrivate.p_get_paymentOrder_by_id campaign paymentId));
                val taskId = (get_PaymentOrder_taskId paymentOrder);
                val task = (Option.valOf (ContractPrivate.p_get_campaign_task_by_task_id campaign taskId));
            in
                if sender = (get_campaign_bankAddress campaign) then
                    if (get_campaign_phase campaign) = PhaseTasks then
                        if ((get_task_taskStatus task) = Confirmed) then
                            SOME (SCCampaign 
                                (ContractPrivate.p_update_Campaign_tasks (ContractPrivate.p_update_paymentOrder_by_id campaign (set_PaymentOrder_paymentStatus paymentOrder PaymentCompleted) paymentId) (set_task_taskStatus task TaskCompleted) taskId)
                            )
                        else
                            SOME (SCCampaign 
                                (ContractPrivate.p_update_paymentOrder_by_id campaign (set_PaymentOrder_paymentStatus paymentOrder PaymentCompleted) paymentId) 
                            )
                    else
                        NONE ( "Action is not allowed at this point.")
                else 
                    NONE ( "Only bank allowed to do this action.")
            end;
        val validated = (validate_params params campaign);
        
    in
        if Option.isNone (get_err validated) then 
            action context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun performTask context params campaign = 
    let 
        val sender = (get_context_msgSender context);
        val requestTime = (get_context_blockNum context);

        fun validate_params params campaign = 
            let
                val taskId = (scvalue_to_int (List.nth params 0));
            in 
                if (Option.isSome taskId) then 
                    if Option.isSome (ContractPrivate.p_get_campaign_task_by_task_id campaign (Option.valOf taskId)) then
                        SOME (SCBool True)
                    else
                        NONE "Task does not exist."
                else
                    NONE "Parse param error: taskId. Incorrect type."
            end;

        fun action context params campaign = 
            let
                val taskId = (Option.valOf (scvalue_to_int (List.nth params 0)));
                val task = (Option.valOf(ContractPrivate.p_get_campaign_task_by_task_id campaign taskId));
            in
                if sender = (get_person_addr (get_task_worker task)) then
                    if (get_campaign_phase campaign) = PhaseTasks then
                        if (get_task_taskStatus task) = GasRequested then
                            SOME (SCCampaign (ContractPrivate.p_update_Campaign_tasks campaign (set_task_taskStatus task Performing) taskId))
                        else 
                            NONE ( "No gas requests.")
                    else
                        NONE ( "Action is not allowed at this point")
                else 
                    NONE ( "Only worker allowed to do this action.")
            end;
        
        val validated = (validate_params params campaign);
    in
        if Option.isNone (get_err validated) then 
            action context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun taskCompleted context params campaign = 
    let 
        val sender = (get_context_msgSender context);
        val suppliedTime = (get_context_blockNum context);

        fun validate_params params campaign = 
            let
                val taskId = (scvalue_to_int (List.nth params 0));
                val suppliedGas = (scvalue_to_int (List.nth params 1));

            in 
                if (Option.isSome taskId) then 
                    if Option.isSome (ContractPrivate.p_get_campaign_task_by_task_id campaign (Option.valOf taskId)) then
                        SOME (SCBool True)
                    else
                        NONE "Task does not exist."
                else
                    NONE "Parse param error: taskId. Incorrect type."
            end;

        fun action context params campaign = 
            let
                val taskId = (Option.valOf (scvalue_to_int (List.nth params 0)));
                val suppliedGas = (Option.valOf (scvalue_to_int (List.nth params 1)));
                val task = (Option.valOf(ContractPrivate.p_get_campaign_task_by_task_id campaign taskId));
            in
                if sender = (get_person_addr (get_task_worker task)) then
                    if (get_campaign_phase campaign) = PhaseTasks then
                        if (get_task_taskStatus task) = Performing then
                            SOME (SCCampaign (ContractPrivate.p_update_Campaign_tasks campaign 
                                                (set_task_taskStatus 
                                                    (set_task_suppliedGas 
                                                        (set_task_suppliedTime task suppliedTime) 
                                                        suppliedGas) 
                                                    TaskCompleted) 
                                                taskId))
                        else 
                            NONE (  "No one is perfroming the task.")
                    else
                        NONE ( "Action is not allowed at this point")
                else 
                    NONE ( "Only worker allowed to do this action.")
            end;
        val validated = (validate_params params campaign);
    in
        if Option.isNone (get_err validated) then 
            action context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun confirmTask context params campaign = 
    let 
        val sender = (get_context_msgSender context);
        val suppliedTime = (get_context_blockNum context);

        fun validate_params params campaign = 
            let
                val taskId = (scvalue_to_int (List.nth params 0));
            in 
                if (Option.isSome taskId) then 
                    if Option.isSome (ContractPrivate.p_get_campaign_task_by_task_id campaign (Option.valOf taskId)) then
                        if (Option.isSome (ContractPrivate.calculateLastPrice campaign)) then 
                            SOME (SCBool True)
                        else
                            NONE "No approved prices."
                    else
                        NONE "Task does not exist."
                else
                    NONE "Parse param error: taskId. Incorrect type."
            end;
        
        fun action context params campaign = 
            let
                val taskId = (Option.valOf (scvalue_to_int (List.nth params 0)));
                val task = (Option.valOf(ContractPrivate.p_get_campaign_task_by_task_id campaign taskId));
                val direction = 
                    if (get_task_suppliedGas task) < (get_task_totalGas task) then
                        if (get_task_paymentType task) = Pre then
                            False
                        else
                            True
                    else
                        True;

                fun make_payment_order task =
                    let 
                        val suppliedGas = (get_task_suppliedGas task);
                        val totalGas = (get_task_totalGas task);
                        val price = Option.valOf (ContractPrivate.calculateLastPrice campaign);
                    in
                        if (get_task_paymentType task) = Pre then 
                            if suppliedGas < totalGas then
                                (PaymentOrder (( totalGas - suppliedGas )* price) 0 0 taskId WaitingForPayment direction)
                            else
                                (PaymentOrder (( suppliedGas - totalGas ) * price) 0 0 taskId WaitingForPayment direction)
                        else if (get_task_paymentType task) = Post then 
                            (PaymentOrder ( suppliedGas * price) 0 0 taskId WaitingForPayment direction)
                        else 
                            (PaymentOrder ( suppliedGas * price) (get_task_paymentTime task) 0 taskId WaitingForPayment direction)
                    end;

                (* Campaign -> Campaign *)
                fun add_payment_order campaign order = 
                    set_campaign_paymentOrders campaign (( get_campaign_paymentOrders campaign) @ [order])
            in
                if sender = (get_person_addr (get_task_captain task)) then
                    if (get_campaign_phase campaign) = PhaseTasks then
                        if (get_task_taskStatus task) = TaskCompleted then
                            SOME (SCCampaign (add_payment_order 
                                                (ContractPrivate.p_update_Campaign_tasks campaign (set_task_taskStatus task Confirmed) taskId) 
                                                (make_payment_order task )))
                        else 
                            NONE ( "No gas requests.")
                    else
                        NONE ( "Action is not allowed at this point")
                else 
                    NONE ( "Only captain allowed to do this action.")
            end;

        val validated = (validate_params params campaign);
    in
        if Option.isNone (get_err validated) then 
            action context params campaign
        else
            validated
    end;

end;