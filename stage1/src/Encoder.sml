	(* byte_array -> (Word8.word list) *)
fun byte_array_to_w8list byteArray = 
  let 
    val count = ((Word8Array.length byteArray)-1)
    fun copy_from_array w8Array n = 
      if n = 0 then [Word8Array.sub w8Array n] else (Word8Array.sub w8Array n)::(copy_from_array w8Array (n-1))
  in
    List.rev (copy_from_array byteArray count)
  end;

(* string -> byte_array *)
fun string_to_w8array str = 
  let
    val newW8Array = Word8Array.array (String.size str) (Word8.fromInt 1)
  in
    (Word8Array.copyVec str 0 (String.size str) newW8Array 0 ; newW8Array)
  end;

(*(Word8.word list) -> Word8.word *)
fun w8CoW8 value = Word8.fromInt (List.length value) ;

(* SCType -> (Word8.word list) -> (Word8.word list) *)
fun encodeType someType value = 
    case someType of
          TypeString => [Word8.fromInt 1] @ value
        | TypeInt => [Word8.fromInt 2] @ value
        | TypeBool => [Word8.fromInt 3] @ value
        | TypeAgreement => [Word8.fromInt 4] @ value
        | TypeTask => [Word8.fromInt 5] @ value
        | TypeNegotiation => [Word8.fromInt 6] @ value
        | TypePriceChange => [Word8.fromInt 7] @ value
        | TypePaymentOrder => [Word8.fromInt 8] @ value
        | TypeCampaign => [Word8.fromInt 9] @ value
        | TypePerson => [Word8.fromInt 10] @ value 
        | TypeList => [Word8.fromInt 11] @ value  
        | TypePaymentStatus => [Word8.fromInt 12] @ value 
        | TypeAgreementDetails => [Word8.fromInt 13] @ value 
        | TypePhase => [Word8.fromInt 14] @ value 
        | TypeTaskStatus => [Word8.fromInt 15] @ value 
        | TypePaymentType => [Word8.fromInt 16] @ value ;

(* Negotiation -> (Word8.word list) *)
fun encodeNegotiation negotiation =
    let
      fun negotiation_to_int value =
        case value of
           NotSet => 1
         | WaitingCustomer => 2 
         | WaitingSupplier => 3  
         | NegotiationRejected => 4
         | NegotiationApproved => 5
    in
       encodeType TypeNegotiation ( [(w8CoW8 [(Word8.fromInt (negotiation_to_int negotiation))])] @ [(Word8.fromInt (negotiation_to_int negotiation))])
    end;

(* PaymentStatus -> (Word8.word list) *)
fun encodePaymentStatus paymentStatus =
    let
      fun paymentStatus_to_int value =
        case value of
           WaitingForPayment => 1
         | PaymentCompleted => 2 
         | PaymentRejected => 3  
    in
       encodeType TypePaymentStatus ( [(w8CoW8 [(Word8.fromInt (paymentStatus_to_int paymentStatus))])] @ [(Word8.fromInt (paymentStatus_to_int paymentStatus))])
    end;

(* bool -> (Word8.word list) *)
fun encodeBool value = 
    if value = True then 
        encodeType TypeBool ( [(w8CoW8 [(Word8.fromInt 1)])] @ [(Word8.fromInt 1)])
    else 
        encodeType TypeBool ( [(w8CoW8 [(Word8.fromInt 0)])] @ [(Word8.fromInt 0)]);

(* Phase -> (Word8.word list) *)
fun encodePhase phase =
    let
      fun phase_to_int value =
        case value of
           PhaseAgreement => 1
         | PhaseTasks => 2 
         | PhaseDeclined => 3  
    in
       encodeType TypePhase ( [(w8CoW8 [(Word8.fromInt (phase_to_int phase))])] @ [(Word8.fromInt (phase_to_int phase))])
    end;

(* Phase -> (Word8.word list) *)
fun encodeTaskStatus taskStatus =
    let
      fun taskStatus_to_int value =
        case value of
           TaskNotAccepted => 1
         | TaskAccepted => 2 
         | TaskReadyToPerform => 3  
         | GasRequested => 4
         | Performing => 5
         | Confirmed => 6
         | TaskCompleted => 7
    in
       encodeType TypeTaskStatus ( [(w8CoW8 [(Word8.fromInt (taskStatus_to_int taskStatus))])] @ [(Word8.fromInt (taskStatus_to_int taskStatus))])
    end;

(* PaymentType -> (Word8.word list) *)
fun encodePaymentType paymentType =
    let
      fun paymentType_to_int value =
        case value of
           Pre => 1
         | Post => 2 
         | Delayed => 3  
    in
       encodeType TypePaymentType ( [(w8CoW8 [(Word8.fromInt (paymentType_to_int paymentType))])] @ [(Word8.fromInt (paymentType_to_int paymentType))])
    end;

(* (Word8.word list) -> int -> (Word8.word list) *)
fun encode_position w8list position = 
    case w8list of
       [] => []
     | (x::tail) => x::(Word8.fromInt position)::tail;

(* string -> (Word8.word list) *)
fun encodeString str = encodeType TypeString ( [(w8CoW8 (byte_array_to_w8list (string_to_w8array str)))] @ (byte_array_to_w8list (string_to_w8array str)));

(* int -> (Word8.word list) *)
fun encodeInt number = 
    let
      fun encodeIntlocal integer = 
        if integer = 0 then [] 
        else [(Word8.fromInt (Int.mod integer 256))] @ (encodeIntlocal (Int.div integer 256))
      val w8l = encodeIntlocal number
    in
     encodeType TypeInt ([(w8CoW8 w8l)] @ w8l)
    end;

(* Person -> (Word8.word list) *)
fun encodePerson person = 
    case person of
     (Person addr name) => 
         let
          val w8fields = ((encode_position (encodeInt addr) 1) @ 
                          (encode_position (encodeString name) 2))
                         
        in
          encodeType TypePerson ( [w8CoW8 w8fields] @ w8fields )
        end;

(* AgreementDetails -> (Word8.word list) *)
fun encodeAgreementDetails agreementDetails = 
    case agreementDetails of
     (AgreementDetails details bankAddress) => 
        let
          val w8fields = (encode_position (encodeString details) 1) @ 
                         (encode_position (encodeInt bankAddress) 2) 
        in
          encodeType TypeAgreementDetails ( [w8CoW8 w8fields] @ w8fields )
        end;

(* PriceChange -> (Word8.word list) *)
fun encodePriceChange priceChange = 
    case priceChange of
     (PriceChange price negotiation startTime) => 
        let
          val w8fields = (encode_position (encodeInt price) 1) @ 
                         (encode_position (encodeNegotiation negotiation) 2) @ 
                         (encode_position (encodeInt startTime) 3) 
        in
          encodeType TypePriceChange ( [w8CoW8 w8fields] @ w8fields )
        end;

(* PaymentOrder -> (Word8.word list) *)
fun encodePaymentOrder paymentOrder = 
    case paymentOrder of
     (PaymentOrder amount paymentTime paymentId taskId paymentStatus direction) => 
        let
          val w8fields = (encode_position (encodeInt amount) 1) @ 
                         (encode_position (encodeInt paymentTime) 2) @ 
                         (encode_position (encodeInt paymentId) 3) @ 
                         (encode_position (encodeInt taskId) 4) @ 
                         (encode_position (encodePaymentStatus paymentStatus) 5) @
                         (encode_position (encodeBool direction) 6)
        in
          encodeType TypePaymentOrder ( [w8CoW8 w8fields] @ w8fields )
        end;

(* Agreement -> (Word8.word list) *)
fun encodeAgreement agreement = 
    case agreement of
     (Agreement negotiation customer supplier details) => 
        let
          val w8fields = (encode_position (encodeNegotiation negotiation) 1) @ 
                         (encode_position (encodePerson customer) 2) @ 
                         (encode_position (encodePerson supplier) 3) @ 
                         (encode_position (encodeAgreementDetails details) 4) 
        in
          encodeType TypeAgreement ( [w8CoW8 w8fields] @ w8fields )
        end;

(* Task -> (Word8.word list) *)
fun encodeTask task = 
    case task of
     (Task taskId negotiation captain worker expectedGas requestedGas suppliedGas totalGas requestTime suppliedTime completionTime paymentTime taskStatus paymentType) => 
        let
          val w8fields = (encode_position (encodeInt taskId) 1) @
                         (encode_position (encodeNegotiation negotiation) 2) @ 
                         (encode_position (encodePerson captain) 3) @ 
                         (encode_position (encodePerson worker) 4) @ 
                         (encode_position (encodeInt expectedGas) 5) @
                         (encode_position (encodeInt requestedGas) 6) @
                         (encode_position (encodeInt suppliedGas) 7) @
                         (encode_position (encodeInt totalGas) 8) @
                         (encode_position (encodeInt requestTime) 9) @
                         (encode_position (encodeInt suppliedTime) 10) @
                         (encode_position (encodeInt completionTime) 11) @
                         (encode_position (encodeInt paymentTime) 12) @
                         (encode_position (encodeTaskStatus taskStatus) 13) @
                         (encode_position (encodePaymentType paymentType) 14)
        in
          encodeType TypeTask ( [w8CoW8 w8fields] @ w8fields )
        end;

(* Campaign -> (Word8.word list) *)
fun encodeCampaign campaign = 
    case campaign of
     (Campaign agreement tasks negotiation priceChanges phase bankAddress paymentOrders) => 
        let
          fun encode_list_task_felds some_list func position = 
            case some_list of
               [] => []
             | (x::tail) => (encode_position (encodeType TypeList (List.tl (func x)) ) position) @ (encode_list_task_felds tail func position)
          fun encode_list_priceChange_felds some_list func position = 
            case some_list of
               [] => []
             | (x::tail) => (encode_position (encodeType TypeList (List.tl (func x))) position) @ (encode_list_priceChange_felds tail func position)
        fun encode_list_paymentOrder_felds some_list func position = 
            case some_list of
               [] => []
             | (x::tail) => (encode_position (encodeType TypeList (List.tl (func x))) position) @ (encode_list_paymentOrder_felds tail func position)
             
          val w8fields = (encode_position (encodeAgreement agreement) 1) @
                         (encode_list_task_felds tasks encodeTask 2) @ 
                         (encode_position (encodeNegotiation negotiation) 3) @ 
                         (encode_list_priceChange_felds priceChanges encodePriceChange 4) @ 
                         (encode_position (encodePhase phase) 5) @
                         (encode_position (encodeInt bankAddress) 6) @
                         (encode_list_paymentOrder_felds paymentOrders encodePaymentOrder 7)
        in
          encodeType TypeCampaign ( [w8CoW8 w8fields] @ w8fields )
        end;

(* SCValue -> (Word8.word list) *)
fun encodeValue value = 
    case value of 
       SCString str => encodeString str
     | SCInt integer => encodeInt integer
     | SCBool someBool => encodeBool someBool
     | SCNegotiation negotiation => encodeNegotiation negotiation
     | SCPaymentStatus paymentStatus => encodePaymentStatus paymentStatus
     | SCAgreementDetails agreementDetails => encodeAgreementDetails agreementDetails
     | SCPerson person => encodePerson person
     | SCPaymentOrder paymentOrder => encodePaymentOrder paymentOrder
     | SCPriceChange priceChange => encodePriceChange priceChange 
     | SCPhase phase => encodePhase phase 
     | SCTaskStatus taskStatus => encodeTaskStatus taskStatus 
     | SCPaymentType paymentType => encodePaymentType paymentType 
     | SCAgreement agreement => encodeAgreement agreement 
     | SCTask task => encodeTask task 
     | SCCampaign campaign => encodeCampaign campaign;