datatype Negotiation = NotSet | WaitingCustomer | WaitingSupplier | NegotiationRejected | NegotiationApproved ;
datatype PaymentStatus = WaitingForPayment | PaymentCompleted | PaymentRejected ;
datatype Phase = PhaseAgreement | PhaseTasks | PhaseDeclined ;
datatype TaskStatus = TaskNotAccepted | TaskAccepted | TaskReadyToPerform | GasRequested | Performing | Confirmed | TaskCompleted ;
datatype PaymentType = Pre | Post | Delayed ;

datatype Context = Context int int (Word8.word list);
fun get_context_msgSender (Context msgSender _ _) = msgSender;
fun get_context_blockNum (Context _ blockNum _) = blockNum;
fun get_context_storage (Context _ _ storage) = storage;

datatype Person = Person int string;
(*setters for Person*) 
fun set_person_addr (Person a name) value = Person value name;
fun set_person_name (Person addr a) value = Person addr value;
(*getters for Person*)
fun get_person_addr (Person addr _) = addr;
fun get_person_name (Person _ name) = name;

datatype AgreementDetails = AgreementDetails string int;
(*setters for AgreementDetails*)
fun set_agreementDetails_details (AgreementDetails details a ) value = AgreementDetails value a ;
fun set_agreementDetails_bankAddress (AgreementDetails a bankAddress ) value = AgreementDetails a value ;
(*getters for AgreementDetails*) 
fun get_agreementDetails_details (AgreementDetails details _ ) = details;
fun get_agreementDetails_bankAddress (AgreementDetails _ bankAddress ) = bankAddress;

datatype Agreement = Agreement Negotiation Person Person AgreementDetails;
(*setters for Agreement*)
fun set_agreement_negotiation (Agreement negotiation a b c) value = Agreement value a b c;
fun set_agreement_customer (Agreement a customer b c) value = Agreement a value b c;
fun set_agreement_supplier (Agreement a b supplier c) value = Agreement a b value c;
fun set_agreement_details (Agreement a b c details) value = Agreement a b c value;
(*getters for Agreement*) 
fun get_agreement_negotiation (Agreement negotiation _ _ _) = negotiation;
fun get_agreement_customer (Agreement _ customer _ _) = customer;
fun get_agreement_supplier (Agreement _ _ supplier _) = supplier;
fun get_agreement_details (Agreement _ _ _ details) = details;

datatype PriceChange = PriceChange int Negotiation int;
(*setters for PriceChange*)
fun set_priceChange_price (PriceChange price a b) value = PriceChange value a b;
fun set_priceChange_negotiation (PriceChange a negotiation b) value = PriceChange a value b;
fun set_priceChange_startTime (PriceChange a b startTime) value = PriceChange a b value;
(*getters for PriceChange*)
fun get_priceChange_price (PriceChange price _ _) = price;
fun get_priceChange_negotiation (PriceChange _ negotiation _) = negotiation;
fun get_priceChange_startTime (PriceChange _ _ startTime) = startTime;
datatype PaymentOrder = PaymentOrder int int int int PaymentStatus bool;
(*setters for PaymentOrder*) 
fun set_PaymentOrder_amount (PaymentOrder amount a b c d e) value = PaymentOrder value a b c d e;
fun set_PaymentOrder_paymentTime (PaymentOrder a paymentTime b c d e) value = PaymentOrder a value b c d e;
fun set_PaymentOrder_paymentId (PaymentOrder a b paymentId c d e) value = PaymentOrder a b value c d e;
fun set_PaymentOrder_taskId (PaymentOrder a b c taskId d e) value = PaymentOrder a b c value d e;
fun set_PaymentOrder_paymentStatus (PaymentOrder a b c d paymentStatus e) value = PaymentOrder a b c d value e;
fun set_PaymentOrder_direction (PaymentOrder a b c d e direction) value = PaymentOrder a b c d e value;
(*getters for PaymentOrder*)
fun get_PaymentOrder_amount (PaymentOrder amount _ _ _ _ _) = amount;
fun get_PaymentOrder_paymentTime (PaymentOrder _ paymentTime _ _ _ _) = paymentTime;
fun get_PaymentOrder_paymentId (PaymentOrder _ _ paymentId _ _ _) = paymentId;
fun get_PaymentOrder_taskId (PaymentOrder _ _ _ taskId _ _) = taskId;
fun get_PaymentOrder_paymentStatus (PaymentOrder _ _ _ _ paymentStatus _) = paymentStatus;
fun get_PaymentOrder_direction (PaymentOrder _ _ _ _ _ direction) = direction;

datatype Task = Task int Negotiation Person Person int int int int int int int int TaskStatus PaymentType;
(*setters for Task*)
fun set_task_id             (Task taskID  a b c d e f g h i k m n l) value =        Task value a b c d e f g h i k m n l;
fun set_task_negotiation    (Task a negotiation b c d e f g h i k m n l) value =    Task a value b c d e f g h i k m n l;
fun set_task_captain        (Task a b captain c d e f g h i k m n l) value =        Task a b value c d e f g h i k m n l;
fun set_task_worker         (Task a b c worker d e f g h i k m n l) value =         Task a b c value d e f g h i k m n l;
fun set_task_expectedGas    (Task a b c d expectedGas e f g h i k m n l) value =    Task a b c d value e f g h i k m n l;
fun set_task_requestedGas   (Task a b c d e requestedGas f g h i k m n l) value =   Task a b c d e value f g h i k m n l;
fun set_task_suppliedGas    (Task a b c d e f suppliedGas g h i k m n l) value =    Task a b c d e f value g h i k m n l;
fun set_task_totalGas       (Task a b c d e f g totalGas h i k m n l) value =       Task a b c d e f g value h i k m n l;
fun set_task_requestTime    (Task a b c d e f g h requestTime i k m n l) value =    Task a b c d e f g h value i k m n l;
fun set_task_suppliedTime   (Task a b c d e f g h i suppliedTime k m n l) value =   Task a b c d e f g h i value k m n l;
fun set_task_completionTime (Task a b c d e f g h i k completionTime m n l) value = Task a b c d e f g h i k value m n l;
fun set_task_paymentTime    (Task a b c d e f g h i k m paymentTime n l) value =    Task a b c d e f g h i k m value n l;
fun set_task_taskStatus     (Task a b c d e f g h i k m n taskStatus l) value =     Task a b c d e f g h i k m n value l;
fun set_task_paymentType    (Task a b c d e f g h i k m n l paymentType) value =    Task a b c d e f g h i k m n l value;
(*getters for Task*)
fun get_task_id (Task taskID _ _ _ _ _ _ _ _ _ _ _ _ _) = taskID;
fun get_task_negotiation (Task _ negotiation _ _ _ _ _ _ _ _ _ _ _ _) = negotiation;
fun get_task_captain (Task _ _ captain _ _ _ _ _ _ _ _ _ _ _) = captain;
fun get_task_worker (Task _ _ _ worker _ _ _ _ _ _ _ _ _ _) = worker;
fun get_task_expectedGas (Task _ _ _ _ expectedGas _ _ _ _ _ _ _ _ _) = expectedGas;
fun get_task_requestedGas (Task _ _ _ _ _ requestedGas _ _ _ _ _ _ _ _) = requestedGas;
fun get_task_suppliedGas (Task _ _ _ _ _ _ suppliedGas _ _ _ _ _ _ _) = suppliedGas;
fun get_task_totalGas (Task _ _ _ _ _ _ _ totalGas _ _ _ _ _ _ ) = totalGas;
fun get_task_requestTime (Task _ _ _ _ _ _ _ _ requestTime _ _ _ _ _ ) = requestTime;
fun get_task_suppliedTime (Task _ _ _ _ _ _ _ _ _ suppliedTime _ _ _ _ ) = suppliedTime;
fun get_task_completionTime (Task _ _ _ _ _ _ _ _ _ _ completionTime _ _ _ ) = completionTime;
fun get_task_paymentTime (Task _ _ _ _ _ _ _ _ _ _ _ paymentTime _ _ ) = paymentTime;
fun get_task_taskStatus (Task _ _ _ _ _ _ _ _ _ _ _ _ taskStatus _ ) = taskStatus;
fun get_task_paymentType (Task _ _ _ _ _ _ _ _ _ _ _ _ _ paymentType ) = paymentType;

datatype Campaign = Campaign Agreement (Task list) Negotiation (PriceChange list) Phase int (PaymentOrder list); 
(*setters for Campaign*)
fun set_campaign_agreement (Campaign agreement a b c d e f) value = Campaign value a b c d e f;
fun set_campaign_tasks (Campaign a tasks b c d e f) value = Campaign a value b c d e f;
fun set_campaign_negotiation (Campaign a b negotiation c d e f) value = Campaign a b value c d e f;
fun set_campaign_priceChanges (Campaign a b c priceChanges d e f) value = Campaign a b c value d e f;
fun set_campaign_phase (Campaign a b c d phase e f) value = Campaign a b c d value e f;
fun set_campaign_bankAddress (Campaign a b c d e bankAddress f) value = Campaign a b c d e value f;
fun set_campaign_paymentOrders (Campaign a b c d e f paymentOrders) value = Campaign a b c d e f value;
(*getters for Campaign*)
fun get_campaign_agreement (Campaign agreement _ _ _ _ _ _) = agreement;
fun get_campaign_tasks (Campaign _ tasks _ _ _ _ _) = tasks;
fun get_campaign_negotiation (Campaign _ _ negotiation _ _ _ _) = negotiation;
fun get_campaign_priceChanges (Campaign _ _ _ priceChanges _ _ _) = priceChanges;
fun get_campaign_phase (Campaign _ _ _ _ phase _ _) = phase;
fun get_campaign_bankAddress (Campaign _ _ _ _ _ bankAddress _) = bankAddress;
fun get_campaign_paymentOrders (Campaign _ _ _ _ _ _ paymentOrders) = paymentOrders;