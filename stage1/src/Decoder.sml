fun w8listToArray w8list = 
  let 
    val newW8Array = Word8Array.array ((List.length w8list)) (Word8.fromInt 1)
    fun fillArray (x::tail) w8Array n = 
      if tail = [] then (Word8Array.update w8Array n x; w8Array) else 
        (Word8Array.update w8Array n x; fillArray tail w8Array (n+1))
  in
    fillArray w8list newW8Array 0
  end;

(*(word8)-> SCtype *)
fun decodeType value = 
    case (Word8.toInt value) of
          1 => TypeString 
        | 2 => TypeInt
        | 3 => TypeBool 
        | 4 => TypeAgreement 
        | 5 => TypeTask
        | 6 => TypeNegotiation
        | 7 => TypePriceChange
        | 8 => TypePaymentOrder
        | 9 => TypeCampaign
        | 10 => TypePerson 
        | 11 => TypeList 
        | 12 => TypePaymentStatus
        | 13 => TypeAgreementDetails 
        | 14 => TypePhase 
        | 15 => TypeTaskStatus 
        | 16 => TypePaymentType ; 

(* (Word8.word list) -> int количество байтов содержащих это значение *)
fun countOfW8 (x::tail) = Word8.toInt x ;

(* (Word8.word list), n -> (list w8) первые n символов *)
fun cutTail (x::tail) n = if n = (List.length (x::tail)) then x::tail else (if n = 0 then [] else x::(cutTail tail (n-1))) ;

(* (Word8.word list), n -> (list w8) остаток листа после n символов *)
fun cutHead (x::tail) n = if n = (List.length (x::tail)) then [] else (if n = 0 then x::tail else (cutHead tail (n-1))) ;

(* (Word8.word list) -> String *)
fun decodeString w8list = 
    case w8list of
       [] => ""
     | w8list => (Word8Array.substring (w8listToArray w8list) 0 (Word8Array.length (w8listToArray w8list))) ;

(* (Word8.word list) -> Int -> Int *)
fun decodeInt value n = 
    case value of 
       [] => n
     | (x::xs) => n+((Word8.toInt x)+(256*(decodeInt xs n)));
     
(* Word8.word -> bool *)
fun decodeBool value = 
    case value of 
       [] => False
     | (x::tail) => case (Word8.toInt x) of 
                       0 => False
                     | 1=> True ;

(* (Word8.word list) Negotiation -> Negotiation *)
fun decodeNegotiation w8List negotiation = 
    case w8List of
       [] => negotiation
     | (x::tail) => 
                let
                  fun negotiation_string_fields number_of_field without_number =
                    case number_of_field of
                       1 => NotSet
                     | 2 => WaitingCustomer
                     | 3 => WaitingSupplier
                     | 4 => NegotiationRejected
                     | 5 => NegotiationApproved
                in
                 negotiation_string_fields (Word8.toInt x) tail
                end;

(* (Word8.word list) -> PaymentStatus -> PaymentStatus *)
fun decodePaymentStatus w8List paymentStatus = 
    case w8List of
       [] => paymentStatus
     | (x::tail) => 
                let
                  fun paymentStatus_string_fields number_of_field without_number =
                    case number_of_field of
                       1 => WaitingForPayment
                     | 2 => PaymentCompleted
                     | 3 => PaymentRejected
                in
                 paymentStatus_string_fields (Word8.toInt x) tail
                end; 

(* (Word8.word list) -> Phase -> Phase *)
fun decodePhase w8List phase = 
    case w8List of
       [] => phase
     | (x::tail) => 
                let
                  fun phase_string_fields number_of_field without_number =
                    case number_of_field of
                       1 => PhaseAgreement
                     | 2 => PhaseTasks
                     | 3 => PhaseDeclined
                in
                 phase_string_fields (Word8.toInt x) tail
                end; 

(* (Word8.word list) -> TaskStatus -> TaskStatus *)
fun decodeTaskStatus w8List taskStatus = 
    case w8List of
       [] => taskStatus
     | (x::tail) => 
                let
                  fun taskStatus_string_fields number_of_field without_number =
                    case number_of_field of
                       1 => TaskNotAccepted
                     | 2 => TaskAccepted 
                     | 3 => TaskReadyToPerform
                     | 4 => GasRequested
                     | 5 => Performing
                     | 6 => Confirmed
                     | 7 => TaskCompleted
                in
                 taskStatus_string_fields (Word8.toInt x) tail
                end;

(* (Word8.word list) -> Phase -> Phase *)
fun decodePaymentType w8List paymentType = 
    case w8List of
       [] => paymentType
     | (x::tail) => 
                let
                  fun paymentType_string_fields number_of_field without_number =
                    case number_of_field of
                       1 => Pre
                     | 2 => Post
                     | 3 => Delayed 
                in
                 paymentType_string_fields (Word8.toInt x) tail
                end; 

(* (Word8.word list) -> Person -> Person *)
fun decodePerson w8List person =
    case w8List of
       [] => person
     | (x::tail) => 
          case (decodeType x) of 
             TypeInt =>
               let
                fun person_integer_fields number_of_field without_number =
                  case number_of_field of
                    1 => decodePerson (cutHead (List.tl without_number) (countOfW8 without_number)) (set_person_addr person (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0)) 
               in
                person_integer_fields (Word8.toInt (List.hd tail)) (List.tl tail)
               end
           | TypeString => 
               let
                fun person_string_fields number_of_field without_number =
                  case number_of_field of
                     2 => decodePerson (cutHead (List.tl without_number) (countOfW8 without_number)) (set_person_name person (decodeString (cutTail (List.tl without_number) (countOfW8 without_number))))
               in
                 person_string_fields (Word8.toInt (List.hd tail)) (List.tl tail)
               end;

(* (Word8.word list) -> AgreementDetails -> AgreementDetails *)
fun decodeAgreementDetails w8List agreementDetails =
    case w8List of
       [] => agreementDetails
     | (x::tail) => 
          case (decodeType x) of 
             TypeInt =>
               let
                fun agreementDetails_integer_fields number_of_field without_number =
                  case number_of_field of
                     2 => decodeAgreementDetails (cutHead (List.tl without_number) (countOfW8 without_number)) (set_agreementDetails_bankAddress agreementDetails (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0)) 
               in
                agreementDetails_integer_fields (Word8.toInt (List.hd tail)) (List.tl tail)
               end
           | TypeString => 
               let
                fun agreementDetails_string_fields number_of_field without_number =
                  case number_of_field of
                     1 => decodeAgreementDetails (cutHead (List.tl without_number) (countOfW8 without_number)) (set_agreementDetails_details agreementDetails (decodeString (cutTail (List.tl without_number) (countOfW8 without_number))))
               in
                 agreementDetails_string_fields (Word8.toInt (List.hd tail)) (List.tl tail)
               end;


(* (Word8.word list) PaymentOrder -> PaymentOrder *)
fun decodePaymentOrder  w8List paymentOrder = 
    case w8List of
       [] => paymentOrder
     | (x::tail) => case (decodeType x) of 
             TypeInt => 
               let
                fun paymentOrder_integer_fields number_of_field without_number =
                  case number_of_field of
                     1 => decodePaymentOrder (cutHead (List.tl without_number) (countOfW8 without_number)) (set_PaymentOrder_amount paymentOrder (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0)) 
                   | 2 => decodePaymentOrder (cutHead (List.tl without_number) (countOfW8 without_number)) (set_PaymentOrder_paymentTime paymentOrder (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0)) 
                   | 3 => decodePaymentOrder (cutHead (List.tl without_number) (countOfW8 without_number)) (set_PaymentOrder_paymentId paymentOrder (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
                   | 4 => decodePaymentOrder (cutHead (List.tl without_number) (countOfW8 without_number)) (set_PaymentOrder_taskId paymentOrder (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
               in
                paymentOrder_integer_fields (Word8.toInt (List.hd tail)) (List.tl tail)
               end
           | TypePaymentStatus => 
               let
                fun paymentOrder_PaymentStatus_fields number_of_field without_number =
                  case number_of_field of
                     5 => decodePaymentOrder (cutHead (List.tl without_number) (countOfW8 without_number)) (set_PaymentOrder_paymentStatus paymentOrder (decodePaymentStatus (cutTail (List.tl without_number) (countOfW8 without_number)) (WaitingForPayment)))
               in
                paymentOrder_PaymentStatus_fields (Word8.toInt (List.hd tail)) (List.tl tail)
               end
            | TypeBool => 
               let
                fun paymentOrder_Bool_fields number_of_field without_number =
                  case number_of_field of
                     6 => decodePaymentOrder (cutHead (List.tl without_number) (countOfW8 without_number)) (set_PaymentOrder_direction paymentOrder (decodeBool (cutTail (List.tl without_number) (countOfW8 without_number)) ))
               in
                paymentOrder_Bool_fields (Word8.toInt (List.hd tail)) (List.tl tail)
               end;

(* (Word8.word list) Agreement -> Agreement *)
fun decodeAgreement w8List agreement =
    case w8List of
       [] => agreement
     | (x::tail) => 
        case (decodeType x) of
           TypeNegotiation => 
            let
             fun agreement_Negotiation_fields number_of_field without_number =
                case number_of_field of
                   1 => decodeAgreement (cutHead (List.tl without_number) (countOfW8 without_number)) (set_agreement_negotiation agreement (decodeNegotiation (cutTail (List.tl without_number) (countOfW8 without_number)) (NotSet)))
            in
             agreement_Negotiation_fields (Word8.toInt (List.hd tail)) (List.tl tail)
            end
         | TypePerson => 
            let
             fun agreement_Person_fields number_of_field without_number =
                case number_of_field of
                   2 => decodeAgreement (cutHead (List.tl without_number) (countOfW8 without_number)) (set_agreement_customer agreement (decodePerson (cutTail (List.tl without_number) (countOfW8 without_number)) (Person 0 "")))
                 | 3 => decodeAgreement (cutHead (List.tl without_number) (countOfW8 without_number)) (set_agreement_supplier agreement (decodePerson (cutTail (List.tl without_number) (countOfW8 without_number)) (Person 0 "")))
            in
             agreement_Person_fields (Word8.toInt (List.hd tail)) (List.tl tail)
            end 
         |  TypeAgreementDetails => 
            let
             fun agreement_agreementDetails_fields number_of_field without_number =
                case number_of_field of
                   4 => decodeAgreement (cutHead (List.tl without_number) (countOfW8 without_number)) (set_agreement_details agreement (decodeAgreementDetails (cutTail (List.tl without_number) (countOfW8 without_number)) (AgreementDetails "" 0) )) 
            in
             agreement_agreementDetails_fields (Word8.toInt (List.hd tail)) (List.tl tail)
            end ;

(* (Word8.word list) Task -> Task *)
fun decodeTask w8List task = 
    case w8List of
       [] => task
     | (x::tail) => 
        case (decodeType x) of  
           TypeNegotiation => 
            let
             fun task_Negotiation_fields number_of_field without_number =
                case number_of_field of
                   2 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_negotiation task (decodeNegotiation (cutTail (List.tl without_number) (countOfW8 without_number)) (NotSet)))
            in
             task_Negotiation_fields (Word8.toInt (List.hd tail)) (List.tl tail)
            end
         | TypePerson => 
            let
             fun task_Person_fields number_of_field without_number =
                case number_of_field of
                   3 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_captain task (decodePerson (cutTail (List.tl without_number) (countOfW8 without_number)) (Person 0 "")))
                 | 4 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_worker task (decodePerson (cutTail (List.tl without_number) (countOfW8 without_number)) (Person 0 "")))
            in
             task_Person_fields (Word8.toInt (List.hd tail)) (List.tl tail)
            end 
         | TypeInt =>
             let
              fun task_integer_fields number_of_field without_number =
                case number_of_field of
                   1 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_id task (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
                 | 5 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_expectedGas task (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
                 | 6 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_requestedGas task (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
                 | 7 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_suppliedGas task (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
                 | 8 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_totalGas task (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
                 | 9 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_requestTime task (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
                 | 10 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_suppliedTime task (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
                 | 11 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_completionTime task (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
                 | 12 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_paymentTime task (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
            in
              task_integer_fields (Word8.toInt (List.hd tail)) (List.tl tail)
            end
         | TypeTaskStatus =>
             let
               fun task_TaskStatus_fields number_of_field without_number = 
                case number_of_field of 
                   13 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_taskStatus task (decodeTaskStatus (cutTail (List.tl without_number) (countOfW8 without_number)) (TaskNotAccepted)))
             in
               task_TaskStatus_fields (Word8.toInt (List.hd tail)) (List.tl tail)
             end
         | TypePaymentType =>
             let
               fun task_PaymentType_fields number_of_field without_number = 
                case number_of_field of 
                   14 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_paymentType task (decodePaymentType (cutTail (List.tl without_number) (countOfW8 without_number)) (Pre)))
             in
               task_PaymentType_fields (Word8.toInt (List.hd tail)) (List.tl tail)
             end;

(* (Word8.word list) PriceChange -> PriceChange *)
fun decodePriceChange w8List priceChange = 
    case w8List of
       [] => priceChange
     | (x::tail) => 
          case (decodeType x) of 
             TypeInt =>
               let
                fun priceChange_integer_fields number_of_field without_number =
                  case number_of_field of
                     1 => decodePriceChange (cutHead (List.tl without_number) (countOfW8 without_number)) (set_priceChange_price priceChange (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0)) 
                   | 3 => decodePriceChange (cutHead (List.tl without_number) (countOfW8 without_number)) (set_priceChange_startTime priceChange (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number))0))
               in
                priceChange_integer_fields (Word8.toInt (List.hd tail)) (List.tl tail)
               end
           | TypeNegotiation => 
            let
             fun agreement_Negotiation_fields number_of_field without_number =
                case number_of_field of
                   2 => decodePriceChange (cutHead (List.tl without_number) (countOfW8 without_number)) (set_priceChange_negotiation priceChange (decodeNegotiation (cutTail (List.tl without_number) (countOfW8 without_number)) (NotSet)))
            in
            agreement_Negotiation_fields (Word8.toInt (List.hd tail)) (List.tl tail)
            end;
 
(* (Word8.word list) Campaign -> Campaign *)
fun decodeCampaign w8List campaign = 
    case w8List of
       [] => campaign
     | (x::tail) => 
          case (decodeType x) of 
             TypeAgreement => 
                let
                  fun campaign_Agreement_fields number_of_field without_number =
                   case number_of_field of
                      1 => decodeCampaign (cutHead (List.tl without_number) (countOfW8 without_number)) (set_campaign_agreement campaign (decodeAgreement (cutTail (List.tl without_number) (countOfW8 without_number)) (Agreement NotSet (Person 0 "") (Person 0 "") (AgreementDetails "" 0))))
                in
                  campaign_Agreement_fields (Word8.toInt (List.hd tail)) (List.tl tail)
                end
            | TypeNegotiation => 
                let
                  fun campaign_Negotiation_fields number_of_field without_number =
                   case number_of_field of
                      3 => decodeCampaign (cutHead (List.tl without_number) (countOfW8 without_number)) (set_campaign_negotiation campaign (decodeNegotiation (cutTail (List.tl without_number) (countOfW8 without_number)) (NotSet)))
                in
                  campaign_Negotiation_fields (Word8.toInt (List.hd tail)) (List.tl tail)
                end
            | TypeList =>
                let
                  fun campaign_list_fields number_of_field without_number = 
                   case number_of_field of 
                      2 => decodeCampaign (cutHead (List.tl without_number) (countOfW8 without_number)) (set_campaign_tasks campaign ((get_campaign_tasks campaign) @ [(decodeTask (cutTail (List.tl without_number) (countOfW8 without_number)) (Task 0 NotSet (Person 0 "") (Person 0 "") 0 0 0 0 0 0 0 0 TaskNotAccepted Pre))]))
                    | 4 => decodeCampaign (cutHead (List.tl without_number) (countOfW8 without_number)) (set_campaign_priceChanges campaign ((get_campaign_priceChanges campaign) @ [(decodePriceChange (cutTail (List.tl without_number) (countOfW8 without_number)) (PriceChange 0 NotSet 0 ))]))
                    | 7 => decodeCampaign (cutHead (List.tl without_number) (countOfW8 without_number)) (set_campaign_paymentOrders campaign ((get_campaign_paymentOrders campaign) @ [(decodePaymentOrder (cutTail (List.tl without_number) (countOfW8 without_number)) (PaymentOrder 0 0 0 0 WaitingForPayment True))]))
                in
                  campaign_list_fields (Word8.toInt (List.hd tail)) (List.tl tail)
                end
            | TypePhase => 
                let
                  fun campaign_Phase_fields number_of_field without_number =
                   case number_of_field of
                      5 => decodeCampaign (cutHead (List.tl without_number) (countOfW8 without_number)) (set_campaign_phase campaign (decodePhase (cutTail (List.tl without_number) (countOfW8 without_number)) (PhaseAgreement)))
                in
                  campaign_Phase_fields (Word8.toInt (List.hd tail)) (List.tl tail)
                end
            | TypeInt => 
                let
                  fun campaign_integer_fields number_of_field without_number =
                   case number_of_field of
                      6 => decodeCampaign (cutHead (List.tl without_number) (countOfW8 without_number)) (set_campaign_bankAddress campaign (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
                in
                  campaign_integer_fields (Word8.toInt (List.hd tail)) (List.tl tail)
                end;

(* (list word8)->SCvalue *)
fun decodeValue value = 
    case value of (x::tail) => case (decodeType x) of TypeString =>  SCString (decodeString tail)
            | TypeInt => SCInt (decodeInt tail 0) 
            | TypeBool => SCBool (decodeBool (List.tl tail))
            | TypeAgreement => SCAgreement
                let
                   val newAgreement = Agreement NotSet (Person 0 "") (Person 0 "") (AgreementDetails "" 0)
                in
                    decodeAgreement (List.tl tail) newAgreement
                end 
            | TypeTask => SCTask 
                let
                    val newTask = Task 0 NotSet (Person 0 "") (Person 0 "") 0 0 0 0 0 0 0 0 TaskNotAccepted Pre
                in
                   decodeTask (List.tl tail) newTask
                end 
            | TypeNegotiation => SCNegotiation
                let
                   val newNegotiation = NotSet
                in
                   decodeNegotiation (List.tl tail) newNegotiation
                end 
            | TypePhase => SCPhase
                let
                   val newPhase = PhaseAgreement
                in
                   decodePhase (List.tl tail) newPhase
                end 
            | TypePaymentType => SCPaymentType
                let
                   val newPaymentType = Pre
                in
                   decodePaymentType (List.tl tail) newPaymentType
                end 
            | TypeTaskStatus => SCTaskStatus 
                let
                   val newTaskStatus = TaskNotAccepted
                in
                   decodeTaskStatus (List.tl tail) newTaskStatus
                end 
            | TypePaymentStatus => SCPaymentStatus     
                let
                   val newPaymentStatus = WaitingForPayment
                in
                   decodePaymentStatus (List.tl tail) newPaymentStatus
                end 
            | TypePriceChange => SCPriceChange 
                let
                   val newPriceChange = PriceChange 0 NotSet 0 
                in
                    decodePriceChange (List.tl tail) newPriceChange
                end 
            | TypePaymentOrder => SCPaymentOrder 
                let
                   val newPaymentOrder = PaymentOrder 0 0 0 0 WaitingForPayment True
                in
                    decodePaymentOrder (List.tl tail) newPaymentOrder
                end 
            | TypeCampaign => SCCampaign 
                let
                   val newCampaign = Campaign (Agreement NotSet (Person 0 "") (Person 0 "") (AgreementDetails "" 0)) [] NotSet [] PhaseAgreement 0 []
                in
                    decodeCampaign (List.tl tail) newCampaign
                end 
            | TypeAgreementDetails => SCAgreementDetails
                let
                   val newAgreementDetails = AgreementDetails "" 0
                in
                    decodeAgreementDetails (List.tl tail) newAgreementDetails
                end 
            | TypePerson => SCPerson 
                let
                   val newPerson = Person 0 ""
                in
                    decodePerson (List.tl tail) newPerson
                end ;