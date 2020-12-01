fun extract somelist foo = 
    case somelist of
    [] => "[]"
    | (x::tail) => if tail = [] then foo x else foo x ^ extract tail foo;

fun negotiation_toString x = 
    if x = NotSet then 
        "NotSet" 
    else if x = WaitingCustomer then 
        "WaitingCustomer"
    else if x = WaitingSupplier then 
        "WaitingSupplier" 
    else if x = NegotiationRejected then 
        "NegotiationRejected" 
    else "NegotiationApproved" 

fun negotiation_toPrettyString x =
    "\t\t\tNgtn: " ^ negotiation_toString x ^ "\n";

(* PaymentStatus -> string *)
fun paymentStatus_toString x = 
    if x = WaitingForPayment then 
        "WaitingForPayment" 
    else if x = PaymentCompleted then 
        "PaymentCompleted"
    else "PaymentRejected" 

(* string -> string *)
fun paymentStatus_toPrettyString x =
    "\t\t\tPaymentStatus: " ^ paymentStatus_toString x ^ "\n";
    
(* Phase -> string *)
fun phase_toString x = 
    case x of
       PhaseAgreement => "PhaseAgreement"
     | PhaseTasks => "PhaseTasks"
     | PhaseDeclined => "PhaseDeclined";

(* string -> string *)
fun phase_toPrettyString x =
    "\t\t\tPhase: " ^ phase_toString x ^ "\n";

(* Phase -> string *)
fun taskStatus_toString x = 
    case x of
       TaskNotAccepted => "TaskNotAccepted"
     | TaskAccepted => "TaskAccepted"
     | TaskReadyToPerform => "TaskReadyToPerform"
     | GasRequested => "GasRequested"
     | Performing => "Performing"
     | Confirmed => "Confirmed"
     | TaskCompleted => "TaskCompleted";

(* string -> string *)
fun taskStatus_toPrettyString x =
    "\t\t\tTaskStatus: " ^ taskStatus_toString x ^ "\n";

(* PaymentType -> string *)
fun paymentType_toString x = 
    case x of
       Pre => "Pre"
     | Post => "Post"
     | Delayed => "Delayed";

(* string -> string *)
fun paymentType_toPrettyString x =
    "\t\t\tPaymentType: " ^ paymentType_toString x ^ "\n";

(* PriceChange string functions *)

(* PriceChange -> string *)
fun priceChange_toString x =
    case x of PriceChange price negotiation startTime =>
    Int.toString price ^
    negotiation_toString negotiation ^
    Int.toString startTime;

(* string -> string *)
fun priceChange_toPrettyString x =
    case x of PriceChange price negotiation startTime =>
    "\t\t\tPrce: " ^ Int.toString price ^ "\n" ^
    "\t\t\tNgtn: " ^ negotiation_toPrettyString negotiation ^ "\n" ^
    "\t\t\tStTm: " ^ Int.toString startTime ^ "\n";

(* Person string functions *) 

(* Person -> string *)
fun person_toString x =
    case x of Person addr name =>
    Int.toString addr ^
    name;  

(* string -> string *)
fun person_toPrettyString x =
    case x of Person addr name =>
    "\t\t\taddr: " ^ Int.toString addr ^ "\n" ^
    "\t\t\tName: " ^ name ^ "\n";

(* AgreementDetails string functions *) 

(* AgreementDetails -> string *)
fun agreementDetails_toString x =
    case x of AgreementDetails details bankAddress =>
    details ^
    Int.toString bankAddress;    

(* string -> string *)
fun agreementDetails_toPrettyString x =
    case x of AgreementDetails details bankAddress =>
    "\t\t\tName: " ^ details ^ "\n" ^
    "\t\t\taddr: " ^ Int.toString bankAddress ^ "\n";

(* Agreement string functions *) 
fun agreement_toString x =
    case x of Agreement negotiation customer supplier details =>
    negotiation_toString negotiation ^
    person_toString customer ^
    person_toString supplier ^
    agreementDetails_toString details;    

fun agreement_toPrettyString x =
    case x of Agreement negotiation customer supplier details =>
    "\t\t\tNgtn: " ^ negotiation_toPrettyString negotiation ^ "\n" ^
    "\t\t\tCstm: " ^ person_toPrettyString customer ^ "\n" ^
    "\t\t\tSplr: " ^ person_toPrettyString supplier ^ "\n" ^
    "\t\t\tDtls: " ^ agreementDetails_toPrettyString details ^ "\n";

(* PaymentOrder string functions *)
fun paymentOrder_toString x =
    case x of PaymentOrder amount paymentTime paymentId taskId paymentStatus direction =>
    Int.toString amount ^
    Int.toString paymentTime ^
    Int.toString paymentId ^
    paymentStatus_toPrettyString paymentStatus;    

fun paymentOrder_toPrettyString x =
    case x of PaymentOrder amount paymentTime paymentId taskId paymentStatus direction =>
    "\t\t\tAmount: " ^ Int.toString amount ^ "\n" ^
    "\t\t\tPmTime: " ^ Int.toString paymentTime ^ "\n" ^
    "\t\t\tPmIndx: " ^ Int.toString paymentId ^ "\n" ^
     paymentStatus_toPrettyString paymentStatus ;

(* Task string functions *)
fun task_toString x =
    case x of Task taskId negotiation captain worker expectedGas requestedGas suppliedGas totalGas requestTime suppliedTime completionTime paymentTime taskStatus paymentType => 
    Int.toString taskId ^ 
    negotiation_toString negotiation ^
    person_toString captain ^ 
    person_toString worker ^ 
    Int.toString expectedGas ^ 
    Int.toString requestedGas ^ 
    Int.toString suppliedGas ^ 
    Int.toString totalGas ^
    Int.toString requestTime ^ 
    Int.toString suppliedTime ^ 
    Int.toString completionTime ^ 
    Int.toString paymentTime ^ 
    taskStatus_toString taskStatus ^
    paymentType_toString paymentType ;

fun task_toPrettyString x =
    case x of Task taskId negotiation captain worker expectedGas requestedGas suppliedGas totalGas requestTime suppliedTime completionTime paymentTime taskStatus paymentType => 
    "\t TskId:" ^ Int.toString taskId ^ "\n" ^ 
    "\t Negotiation:" ^ negotiation_toString negotiation ^ "\n" ^ 
    "\t Captn:\n" ^ person_toPrettyString captain ^ "\n" ^ 
    "\t Workr:\n" ^ person_toPrettyString worker ^ "\n" ^ 
    "\t ExpGs:" ^ Int.toString expectedGas ^ "\n" ^ 
    "\t ReqGs:" ^ Int.toString requestedGas ^ "\n" ^ 
    "\t SupGs:" ^ Int.toString suppliedGas ^ "\n" ^ 
    "\t totalGas:" ^ Int.toString totalGas ^ "\n" ^ 
    "\t ReqTm:" ^ Int.toString requestTime ^ "\n" ^ 
    "\t SupTm:" ^ Int.toString suppliedTime ^ "\n" ^ 
    "\t CmpTm:" ^ Int.toString completionTime ^ "\n" ^ 
    "\t paymentTime:" ^ Int.toString paymentTime ^ "\n" ^
    taskStatus_toPrettyString taskStatus ^
    "\t paymentType:" ^ paymentType_toString paymentType ^ "\n" ;

(* Campaign string functions *)
fun campaign_toString x = (* To DO: Pretty print for agreement |*)
    case x of Campaign agreement tasks negotiation priceChanges phase bankAddress paymentOrders => 
    agreement_toString agreement  ^ 
    extract tasks task_toString ^ 
    negotiation_toPrettyString negotiation ^ 
    extract priceChanges priceChange_toString ^
    phase_toString phase ^  
    Int.toString bankAddress ^
    extract paymentOrders paymentOrder_toString;

fun campaign_toPrettyString x = (* to do Pretty print for agreement |*)
    case x of Campaign agreement tasks negotiation priceChanges phase bankAddress paymentOrders => 
    "Agrmt: " ^ agreement_toPrettyString agreement  ^ "\n" ^
    "Tasks:\n" ^ extract tasks task_toPrettyString^
    "Negot: " ^ negotiation_toPrettyString negotiation ^ "\n" ^
    "PrChn: " ^ extract priceChanges priceChange_toPrettyString ^ "\n" ^
    "Phase: " ^ phase_toPrettyString phase ^ "\n" ^
    "bankAddress: " ^ Int.toString bankAddress ^ "\n" ^ 
    "paymentOrders: " ^ extract paymentOrders paymentOrder_toPrettyString ^ "\n" ;