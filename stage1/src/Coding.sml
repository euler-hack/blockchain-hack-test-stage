datatype SCType = TypeInt 
    | TypeString 
    | TypeBool 
    | TypeAgreement 
    | TypeTask
    | TypeNegotiation
    | TypePriceChange
    | TypePaymentOrder
    | TypeCampaign
    | TypePerson 
    | TypeList 
    | TypePaymentStatus
    | TypeAgreementDetails 
    | TypeTaskStatus 
    | TypePaymentType
    | TypePhase ;
    

datatype SCvalue = SCInt int
    | SCString string
    | SCBool bool
    | SCAgreement Agreement
    | SCTask Task
    | SCNegotiation Negotiation
    | SCPriceChange PriceChange
    | SCPaymentOrder PaymentOrder
    | SCCampaign Campaign
    | SCPerson Person
    | SCPaymentStatus PaymentStatus 
    | SCTaskStatus TaskStatus 
    | SCAgreementDetails AgreementDetails 
    | SCPaymentType PaymentType
    | SCPhase Phase ;

(* SCValue -> int option *)
fun scvalue_to_int x = 
    case x of 
    (SCInt x) => Some x
    | _ => None;
(* SCValue -> string option*)
fun scvalue_to_string x =
    case x of 
    (SCString x) => Some x
    | _ => None;
(* SCValue -> bool option*)
fun scvalue_to_bool x =
    case x of 
    (SCBool x) => Some x
    | _ => None;
(* SCValue -> AgreementDetails option*)
fun scvalue_to_agreementDetails x =
    case x of 
    (SCAgreementDetails x) => Some x
    | _ => None;
(*SCValue -> Negotiation option*)
fun scValue_to_negotiation x = 
    case x of 
    (SCNegotiation x) => Some x
    | _ => None;
(*SCValue -> Person option*)
fun scValue_to_person x = 
    case x of 
    (SCPerson x) => Some x
    | _ => None;
(*SCValue -> TaskStatus option*)
fun scValue_to_taskStatus x =
    case x of 
    (SCTaskStatus x) => Some x
    | _ => None;
(*SCValue -> Campaign option*)
fun scValue_to_campaign x =
    case x of 
    (SCCampaign x) => Some x
    | _ => None;
(*SCValue -> PaymentType option*)
fun scValue_to_paymentType x =
    case x of 
    (SCPaymentType x) => Some x
    | _ => None;
(*SCValue -> Task option*)
fun scValue_to_Task x =
    case x of 
    (SCTask x) => Some x
    | _ => None;
(*SCValue -> Agreement option*)
fun scValue_to_Agreement x =
    case x of 
    (SCAgreement x) => Some x
    | _ => None;
(*SCValue -> PriceChange option*)
fun scValue_to_PriceChange x =
    case x of 
    (SCPriceChange x) => Some x
    | _ => None;