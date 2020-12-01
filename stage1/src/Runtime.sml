structure RuntimePrivate = 
struct
    (* int -> (Word8.word list) -> (SCValue list) -> (SCValue OptionErr)*)
    fun execute f context params campaign = 
        case f of 
        2 => Contract.getAgreement context params campaign
        | 3 => Contract.rejectAgreement context params campaign
        | 4 => Contract.approveAgreement context params campaign
        | 5 => Contract.changeAgreementDetails context params campaign
        | 6 => Contract.getPriceChangeWithNumber context params campaign
        | 7 => Contract.getPriceChangesLength context params campaign
        | 8 => Contract.rejectPrice context params campaign
        | 9 => Contract.approvePrice context params campaign
        | 10 => Contract.declinePrice context params campaign
        | 11 => Contract.createPriceChange context params campaign
        | 12 => Contract.getTask context params campaign
        | 13 => Contract.approveTask context params campaign
        | 14 => Contract.rejectTask context params campaign
        | 15 => Contract.acceptTask context params campaign
        | 16 => Contract.removeTask context params campaign
        | 17 => Contract.addTask context params campaign
        | 18 => Contract.readyToPerformTask context params campaign
        | 19 => Contract.requestGas context params campaign
        | 20 => Contract.paymentCompleted context params campaign
        | 21 => Contract.performTask context params campaign
        | 22 => Contract.taskCompleted context params campaign
        | 23 => Contract.confirmTask context params campaign
        | n => NONE "The function doesn't exist";
end;

structure Runtime = 
struct
    fun call f context params =
    let        
        val storage = (get_context_storage context);

        (*(Word8Word list) -> Campaign option*)
        fun deserialize storage = 
            if storage = [] then 
                None
            else 
                scValue_to_campaign (decodeValue storage);
        
        val campaign = deserialize storage;

        val optionCampaign = 
            if Option.isSome campaign then
                if f <> 1 then
                    (RuntimePrivate.execute f context params (Option.valOf campaign))
                else 
                    NONE "Storage is not empty"
            else if f = 1 then 
                Contract.constructor context params
            else
                NONE "Error deserializtion";        
    in
        if Option.isNone (get_err optionCampaign) then
            if Option.isSome (get optionCampaign) then
                SOME (encodeValue (Option.valOf ( get optionCampaign )))
            else
                RET (encodeValue (Option.valOf ( get_ret_state optionCampaign ))) (encodeValue (Option.valOf ( get_ret_val optionCampaign )))
        else
            NONE (Option.valOf (get_err optionCampaign))
    end;
end;