%%%-------------------------------------------------------------------
%%% @author simonxu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Apr 2016 09:48
%%%-------------------------------------------------------------------
-author("simonxu").

-type up_version() :: byte5().        % NS5
-type up_encoding() :: byte5().      % UTF-8
-type up_certId() :: bytes().    % N1..128
-type up_signature() :: bytes().  % ANS1..1024
-type up_signMethod() :: byte2().    % N2:01
-type up_txnType() :: byte2().        % N2
-type up_txnSubType() :: byte2().    % N2
-type up_bizType() :: byte6().        % N6
-type up_channelType() :: byte2().    % N2
-type up_frontUrl() :: bytes().    % ANS1..256
-type up_backUrl() :: bytes().      % ANS1..256
-type up_accessType() :: byte1().      % N1
-type up_merId() :: byte15().        % AN15
-type up_subMerId() :: any().      % AN5..15
-type up_subMerName() :: any().    % ANS1..40
-type up_subMerAbbr() :: any().    % ANS1..16
-type up_orderId() :: byte8_up().      % AN8..32
-type up_txnTime() :: byte14().      % YYYYMMDDhhmmss
-type up_accType() :: byte2().        % N2
-type up_accNo() :: any().          % AN1..512
%-type up_txnAmt() :: bytes().          % N1..12
-type up_txnAmt() :: non_neg_integer().
-type up_currencyCode() :: byte3().    % N3
-type up_customerInfo() :: any().
-type up_orderTimeout() :: any().
-type up_payTimeout() :: any().
-type up_termId() :: byte8().          % AN8
-type up_reqReserved() :: bytes().
-type up_reserved() :: any().
-type up_riskRateInfo() :: any().
-type up_encryptCertId() :: any().
-type up_frontFailUrl() :: any().
-type up_instalTransInfo() :: any().
-type up_defaultPayType() :: byte4().    % N4
-type up_issInsCode() :: any().        % AN1..20
-type up_supPayType() :: any().
-type up_userMac() :: any().
-type up_customerIp() :: any().
-type up_cardTransData() :: any().
-type up_orderDesc() :: any().

%% reply & inform packet
-type up_respCode() :: byte2().
-type up_respMsg() :: any().
-type up_queryId() :: any().
-type up_settleAmt() :: non_neg_integer().
-type up_settleCurrencyCode() :: any().
-type up_settleDate() :: byte4().
-type up_traceNo() :: any().
-type up_traceTime() :: any().

%% inform packet
-type up_exchangeDate() :: any().
-type up_exchangeRate() :: any().
-type up_payCardNo() :: any().
-type up_payCardIssueName() :: any().
-type up_bindId() :: any().


-type up_issuerIndentifyMode() :: byte1().


%% file transfer
-type up_file_transfer_type() :: byte2().

-define(UP_VERSION,<<"5.0.0">>).
-define(UP_ENCODING,<<"UTF-8">>).
-define(UP_SIGNMETHOD,<<"01">>).
-define(UP_TXNTYPE_FILE_TRANSFTER,<<"76">>).
-define(UP_TXNSUBTYPE_RECONCILE_FILE_DOWNLOAD,<<"01">>).
-define(UP_BIZTYPE_FILE_TRASNFER,<<"000000">>).
-define(UP_ACCESSTYPE_NORMAL,<<"0">>).
-define(UP_FILETYPE_NORMAL,<<"00">>).

