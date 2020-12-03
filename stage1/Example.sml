fun print_campaign result =
    if Option.isNone (get_err result) then
	print(campaign_toPrettyString ((Option.valOf( scValue_to_campaign( decodeValue(Option.valOf(get ( result ))))))))
    else
	print(Option.valOf(get_err ( result )))

fun init msgSender blockNum params =
    let
	val storage = []
	val context = Context msgSender blockNum storage;
	val result = Runtime.call 1 context params;
    in
	print ("** init **\n");
	print_campaign (result);
	result
    end;

fun rejectAgreement prevRes msgSender blockNum =
    let
	val storage = Option.valOf (get (prevRes))
	val context = Context msgSender blockNum storage;
	val result = Runtime.call 3 context [];
    in
	print("\n** rejectAgreement **\n");
	print_campaign (result);
	result
    end;

fun approveAgreement prevRes msgSender blockNum =
    let
	val storage = Option.valOf (get (prevRes))
	val context = Context msgSender blockNum storage;
	val result = Runtime.call 4 context [];
    in
	print("\n** approveAgreement **\n");
	print_campaign (result);
	result
    end;

fun createPriceChange prevRes msgSender blockNum params =
    let
	val storage = Option.valOf (get (prevRes))
	val context = Context msgSender blockNum storage;
	val result = Runtime.call 11 context params;
    in
	print("\n** createPriceChange **\n");
	print_campaign (result);
	result
    end;

fun approvePrice prevRes msgSender blockNum =
    let
	val storage = Option.valOf (get (prevRes))
	val context = Context msgSender blockNum storage;
	val result = Runtime.call 9 context [];
    in
	print("\n** approvePrice **\n");
	print_campaign (result);
	result
    end;

fun addTask prevRes msgSender blockNum params =
    let
	val storage = Option.valOf (get (prevRes))
	val context = Context msgSender blockNum storage;
	val result = Runtime.call 17 context params;
    in
	print("\n** addTask **\n");
	print_campaign (result);
	result
    end;

fun approveTask prevRes msgSender blockNum params =
    let
	val storage = Option.valOf (get (prevRes))
	val context = Context msgSender blockNum storage;
	val result = Runtime.call 13 context params;
    in
	print("\n** approveTask **\n");
	print_campaign (result);
	result
    end;

fun acceptTask prevRes msgSender blockNum params =
    let
	val storage = Option.valOf (get (prevRes))
	val context = Context msgSender blockNum storage;
	val result = Runtime.call 15 context params;
    in
	print("\n** acceptTask **\n");
	print_campaign (result);
	result
    end;

fun readyToPerformTask prevRes msgSender blockNum params =
    let
	val storage = Option.valOf (get (prevRes))
	val context = Context msgSender blockNum storage;
	val result = Runtime.call 18 context params;
    in
	print("\n** readyToPerformTask **\n");
	print_campaign (result);
	result
    end;

fun requestGas prevRes msgSender blockNum params =
    let
	val storage = Option.valOf (get (prevRes))
	val context = Context msgSender blockNum storage;
	val result = Runtime.call 19 context params;
    in
	print("\n** requestGas **\n");
	print_campaign (result);
	result
    end;

fun performTask prevRes msgSender blockNum params =
    let
	val storage = Option.valOf (get (prevRes))
	val context = Context msgSender blockNum storage;
	val result = Runtime.call 21 context params;
    in
	print("\n** performTask **\n");
	print_campaign (result);
	result
    end;

fun taskCompleted prevRes msgSender blockNum params =
    let
	val storage = Option.valOf (get (prevRes))
	val context = Context msgSender blockNum storage;
	val result = Runtime.call 22 context params;
    in
	print("\n** taskCompleted **\n");
	print_campaign (result);
	result
    end;

fun confirmTask prevRes msgSender blockNum params =
    let
	val storage = Option.valOf (get (prevRes))
	val context = Context msgSender blockNum storage;
	val result = Runtime.call 23 context params;
    in
	print("\n** confirmTask **\n");
	print_campaign (result);
	result
    end;

fun main () =
    let
	val blockNum = 1000;

        val customerAddr = 1337;
	val customerName = "Aviacompany";
	val supplierAddr = 1338;
	val supplierName = "Fuel company";
	val agreementDetails = "This very cool agreement";
	val bankAddr = 1339;
	val initParameters = [SCInt customerAddr, SCString customerName, SCInt supplierAddr, SCString supplierName, SCString agreementDetails, SCInt bankAddr]
	val step1 = init customerAddr blockNum initParameters;

	val step2 = approveAgreement step1 supplierAddr (blockNum + 1);

	val price = 100;
	val negotiation_p = WaitingCustomer;
	val startTime = 200;
	val priceParameters = [
	    SCInt price,
	    SCNegotiation negotiation_p,
	    SCInt startTime
	];
	val step3 = createPriceChange step2 supplierAddr (blockNum + 2) priceParameters;

	val step4 = approvePrice step3 customerAddr (blockNum + 3);

	val taskId = 999;
	val negotiation_t = NotSet;
	val captainAddr = 1340;
	val captainName = "Ivanov";
	val workerAddr = 1341;
	val workerName = "Petrov";
	val expectedGas = 100;
	val requestedGas = 100;
	val suppliedGas = 100;
	val totalGas = 100;
	val requestTime = 200;
	val suppliedTime = 200;
	val completionTime = 200;
	val paymentTime = 200;
	val taskStatus = TaskReadyToPerform;
	val paymentType = Pre;
	val taskParams = [
	    SCInt taskId,
	    SCNegotiation negotiation_t,
	    SCInt captainAddr,
	    SCString captainName,
	    SCInt workerAddr,
	    SCString workerName,
	    SCInt expectedGas,
	    SCInt requestedGas,
	    SCInt suppliedGas,
	    SCInt totalGas,
	    SCInt requestTime,
	    SCInt suppliedTime,
	    SCInt completionTime,
	    SCInt paymentTime,
	    SCTaskStatus taskStatus,
	    SCPaymentType paymentType
	];
	val step5 = addTask step4 customerAddr (blockNum + 4) taskParams;

	val step6 = approveTask step5 supplierAddr (blockNum + 5) [SCInt taskId];

	val step7 = acceptTask step6 workerAddr (blockNum + 6) [SCInt taskId];

	val requestGasParams = [
	    SCInt taskId,
	    SCInt 100,
	    SCInt 200
	];

	val step8 = readyToPerformTask step7 workerAddr (blockNum + 7) [SCInt taskId]

	val step9 = requestGas step8 captainAddr (blockNum + 8) requestGasParams;

	val step10 = performTask step9 workerAddr (blockNum + 9) [SCInt taskId];

	val step11 = taskCompleted step10 workerAddr (blockNum + 10) [SCInt taskId, SCInt suppliedGas];

	val step12 = confirmTask step11 captainAddr (blockNum + 11) [SCInt taskId];

    in
	step12
    end;


main ();
