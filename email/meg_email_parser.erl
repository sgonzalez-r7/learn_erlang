-module(meg_email_parser).

% -export([parse/1]).
-compile([export_all]).

%% Description TODO: edit
%%
%% Parameters:
%% Email - a raw parent email, which has the following data structure:
%%         {Type, SubType, Headers, Parameters, Body}, see
%%         https://github.com/Vagabond/gen_smtp/blob/master/src/mimemail.erl
%%         for details of the API.


parse(Raw) ->
  Parent_email = mimemail:decode(Raw),
  parse_email(Parent_email).

parse_email({_Type, _SubType, _Headers, _Parameters, Body}) ->
  case Body of
    Parts when is_list(Parts) ->
      _Email = find_part({<<"message">>, <<"rfc822">>}, Parts);
    _ ->
      none
  end.

find_part({Type, SubType}, Parts) ->
  Found = fun({T, ST, _H, _P, _B}) -> T =:= Type andalso ST =:= SubType end,
  Found_parts = lists:filter(Found, Parts),
  case Found_parts of
    [] ->
      none;
    _ ->
      hd(Found_parts)
  end.

raw_email() ->
  "Received: from DM2PR0301MB0639.namprd03.prod.outlook.com (10.160.96.13) by\r\n BY2PR0301MB0632.namprd03.prod.outlook.com (10.160.63.12) with Microsoft SMTP\r\n Server (version=TLS1_2, cipher=TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384_P384) id\r\n 15.1.534.14 via Mailbox Transport; Tue, 12 Jul 2016 20:38:15 +0000\r\nReceived: from BN3PR0301CA0064.namprd03.prod.outlook.com (10.160.152.160) by\r\n DM2PR0301MB0639.namprd03.prod.outlook.com (10.160.96.13) with Microsoft SMTP\r\n Server (version=TLS1_2, cipher=TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384_P384) id\r\n 15.1.539.14; Tue, 12 Jul 2016 20:38:13 +0000\r\nReceived: from BN1AFFO11FD037.protection.gbl (2a01:111:f400:7c10::172) by\r\n BN3PR0301CA0064.outlook.office365.com (2a01:111:e400:401e::32) with Microsoft\r\n SMTP Server (version=TLS1_2,\r\n cipher=TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384_P384) id 15.1.539.14 via\r\n Frontend Transport; Tue, 12 Jul 2016 20:38:13 +0000\r\nAuthentication-Results: spf=pass (sender IP is 209.85.215.42)\r\n smtp.mailfrom=gmail.com; rapid7.com; dkim=pass (signature was verified)\r\n header.d=gmail.com;rapid7.com; dmarc=pass action=none\r\n header.from=gmail.com;rapid7.com; dkim=pass (signature was verified)\r\n header.d=gmail.com;\r\nReceived-SPF: Pass (protection.outlook.com: domain of gmail.com designates\r\n 209.85.215.42 as permitted sender) receiver=protection.outlook.com;\r\n client-ip=209.85.215.42; helo=mail-lf0-f42.google.com;\r\nReceived: from mail-lf0-f42.google.com (209.85.215.42) by\r\n BN1AFFO11FD037.mail.protection.outlook.com (10.58.52.241) with Microsoft SMTP\r\n Server (TLS) id 15.1.523.9 via Frontend Transport; Tue, 12 Jul 2016 20:38:13\r\n +0000\r\nReceived: by mail-lf0-f42.google.com with SMTP id q132so23247228lfe.3\r\n        for <sonny_gonzalez@rapid7.com>; Tue, 12 Jul 2016 13:38:13 -0700 (PDT)\r\nDKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed;\r\n        d=gmail.com; s=20120113;\r\n        h=mime-version:from:date:message-id:subject:to;\r\n        bh=D9DvOkwj75H75msH/FxMNg0fm28tsO8tE18iu+ZrNYc=;\r\n        b=KVtV8Kb+Ugy0llx8Ng3jw4N0vdx5QBN1kgM1s/Md1TPlLuLlabaENRdPNR0LXFsHCo\r\n         9JuUOKKmHZPZl33/K5iE0OLxT0NU0H+gMKgCrUrJYIPWr++rTKcne9wo0rj5nXgH2V2c\r\n         9wskv+JY033mVpC7g6jLDyD3x4iGxn5+6xgd6aKPGsZmmtlXrmJlMQXCHh++AU77OLS1\r\n         rI6M1T5JKE9VMiRyQuK7xZN8kp0U2jsywCniqsK5lFhcM8hzSz1x2DWzWcLVsOJ4FcrB\r\n         Uzsz1UJpcFjCgZfemMf0Iwk1yApphKDl1AI2FTDDDITI1CLb2y3VVMu/5jDTIMErIUDn\r\n         O/7A==\r\nX-Google-DKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed;\r\n        d=1e100.net; s=20130820;\r\n        h=x-gm-message-state:mime-version:from:date:message-id:subject:to;\r\n        bh=D9DvOkwj75H75msH/FxMNg0fm28tsO8tE18iu+ZrNYc=;\r\n        b=B7pdBoknrSYoRL7eDWhtOosnAmZCWLbnF5DEDqWUZEnGer++MmqU6J5m6I6ZGZ5PrV\r\n         20IQZcvK84OJfy7N8qLkZhtzIcvUBfWz/HUXtxpW4rL/qNmqdAw2KAtnjwZIKC/yjCMb\r\n         BsVpjrKdV8+CCze5rSQJ7qNs5IftKCllcMLhM0+/4twcXct2jMWt3UM+k5MlcelukPgu\r\n         NuXIav0z6H3B1Eq/0Fahk22lOX8Jm08aazBzwdnosBu2GPVrsM0/L6w4qay0ZhWther/\r\n         mtZfrTOq/xJ8alJj7DwstQNeMOYCKq3qvcxm5ejhFIr7iMjEgDltBnTys6C/nKFYyxY0\r\n         Ke7w==\r\nX-Gm-Message-State: ALyK8tJl9uDkpuRa8g41zLVK/jHdUcsurKH8LdDC0PsO9ogEYm3vUIYVnpQPvWG7JcBiVcxW61OGD9/JxHNI9g==\r\nX-Received: by 10.25.143.132 with SMTP id r126mr2193580lfd.160.1468355891628;\r\n Tue, 12 Jul 2016 13:38:11 -0700 (PDT)\r\nMIME-Version: 1.0\r\nReceived: by 10.25.216.6 with HTTP; Tue, 12 Jul 2016 13:38:11 -0700 (PDT)\r\nFrom: Sonny Gonzalez <sonnyg.email@gmail.com>\r\nDate: Tue, 12 Jul 2016 15:38:11 -0500\r\nMessage-ID: <CA+9Poanvz6kYKJEw1GgnUXo2VD6djabR7uFNVnPRr9=ndHhQzQ@mail.gmail.com>\r\nSubject: veggie tails\r\nTo: sonny_gonzalez@rapid7.com\r\nReturn-Path: sonnyg.email@gmail.com\r\nX-MS-Exchange-Organization-Network-Message-Id: a50bcf85-9189-4b2b-bd5b-08d3aa94720c\r\nX-EOPAttributedMessage: 0\r\nX-EOPTenantAttributedMessage: 52ec8a0a-dc28-4845-b421-6038854d679f:0\r\nX-MS-Exchange-Organization-MessageDirectionality: Incoming\r\nX-Forefront-Antispam-Report: CIP:209.85.215.42;IPV:NLI;CTRY:US;EFV:NLI;SFV:NSPM;SFS:(6009001)(8156002)(2980300002)(438002)(189002)(199003)(93516999)(1096003)(98316002)(73972006)(3480700004)(59536001)(73392002)(50986999)(8896002)(82202001)(81442002)(61266001)(8676002)(92566002)(221733001)(11100500001)(450100001)(110136002)(956001)(107886002)(7846002)(54356999)(63696999)(189998001)(83322999)(8576002)(9896002)(6806005)(55446002)(2361001)(356003)(42186005)(305945005)(7596002)(2351001)(106466001)(84326002)(76482005)(564344004)(7636002)(87572001)(586003)(86362001)(512874002)(5000100001)(229853001)(246002)(16796002)(358055004);DIR:INB;SFP:;SCL:1;SRVR:DM2PR0301MB0639;H:mail-lf0-f42.google.com;FPR:;SPF:Pass;PTR:mail-lf0-f42.google.com;MX:1;A:1;LANG:en;\r\nX-Microsoft-Exchange-Diagnostics: 1;BN1AFFO11FD037;1:UqfXGeDUAYVRAdJBO8tCfSEhgS2B10WkNiPiE9i1JtNi+/9ZeCjHn7iiYuLI+n9/zzHXkAuC9o9tZp93CKPnj7ZspAa0T2Am5Fp12lIoTtHnAnONeEHhXdZaYXby92MsKRc4gXsdYMOrIhk6zX3Kf5vU2upiPbwO9cn5mbWahG/GXkl9WAyxowjvdhGAje/Bvb2jO0RgecloW2TazLY786VGDH4UadBWo0yLh+LMbr92oAOoMwJLYn28sDQT0MkexQaZo7Qn1O09qjoN1hoJrqynSVrLzjB4FHVl4A3+IA4aGvBepWmS5YQ6qzBlKYOqbRD+T11BOmjDAM3VFepdIsqyRpirAkfkQlY50X4Q5IhK093b2gjb8piHNf/IpKjOrAG7FQikTVZ+5hpSHxhfiUl/WGWl1cjSDougsL2o0O4Qjsxl6bw211HkIYFfvwLDzPMozCX3ZJNYJ1msZKHYekQgi/UoTZZtG4SGXfiAaZSZot/sh0EE//dI72FBJZV+imZ9Te3xutQs/FU5nldSa+l2+pFWX1Y3K69BcIr9jVwwG0kALcV93GDxSKJNaqoP\r\nX-MS-Office365-Filtering-Correlation-Id: a50bcf85-9189-4b2b-bd5b-08d3aa94720c\r\nX-Microsoft-Exchange-Diagnostics: 1;DM2PR0301MB0639;2:Ta63v9xsMa+iZcHPYseoXex8bIKyCN8JzzpMSZOweQy6LF3+w4DaGYAG7pAvEuCho5fCS2AUTLAE9YjCE6S0r/dPVZg49v8GTI17HoM7ONM5LE/U3h82wzDUVoSZBVAKenufqDPEF+4yRU+pt5e1Y9JbmZzHRU9WKQGa4gs1fLfKYbDaum4nPF+867MU7ev/;3:CGJtLCvN5Z5LJ2rCS9psQC3p266HOyPDlrYzkW34avjqB+LrF1ZAT4ZPlT9kc1nzhHqadGqH7HU3TC1Qcin3w3/qZyXinDnWl5jMS+1dM6u6vfLNg6buIXc/93Q3lDCt+xXTdgalYg/CyIS9bpBn9bq4S/SdV/JMVffNo2hlBOSgqREwsdxlu5Yp7sdNkVA/rqG2B/4TXDfGpsoDvLoQ0p/2WVtKymB3X7gww0pbdfjcefk1jBQS24lNENTFmzsqmaKzAdsoqr9CdfFIlaZoFqwI8RR2ZNajnveirzTD2p3HyBU/jX6jimrdEwTC8wpBBDdYhtbT9wHwFx1bd0UA6sCUhCCJG0n5sxCqQOhx1F8=\r\nX-DkimResult-Test: Passed\r\nX-Microsoft-Antispam: UriScan:;BCL:0;PCL:0;RULEID:(8251501002)(3001016)(3010002)(71701004)(71702002);SRVR:DM2PR0301MB0639;\r\nX-Microsoft-Exchange-Diagnostics: 1;DM2PR0301MB0639;25:HAglOUBK4u8C+DKft1bi6kqirc7Cml1dor04o23Yu7sAgHCTU4lmGZAqCRbuOqXPqZL7hix3WyuWfp130omM52d0GK/S9F7dIC9qdxPHuv3brbQQ1uqi1v7DbrdbL4QYBh35Uns396jkWqwc0lZS0On4w0njg98vseBAPqxhK4ydHDfCg29Z2/1FLnYKm2sY4UZmTiQ8Wdev8O/epwGxI/K41ReUgCRcF0SjlTwYfLLf73NgRT0VOnN3Ks5VhOE6JKKTpN0JnYFdayrd7ZqIvQTe2xL4Q+wNk/8hSc+DxRk1na8y6szt6x82jcZmbJeWI0z+YJ1T94DlLUN4Kda+VQdhQ9XEjdcu13GGUmfcQudk5PGoPkf3ibhLypXtCz6Al4MRYdNiLTrOcSNNl3R+aIlKgJvFJtpr8GYxEDVvWOY=;31:k/j1CwQq+DtMgUwyr+TFfFcyOrrBJHW/5qBfXGoTRk4vcNtvWOd0jpOnEMdHamLYRa8qVwTsbcAbb3hm3WxbrU8eVHwTi0w6bwCcXWYV1N+rFNWDOFck1ZvmqT/hVn2GwmrYCLNP9LqlKbwMkJ0NPW3XYHEyl3nFZFe7RDILC233VqxAll7nJXV5Dd5BC51uMubicW6exkwM6PBT7eM7ww==\r\nX-MS-Exchange-Organization-AVStamp-Service: 1.0\r\nX-Microsoft-Exchange-Diagnostics: 1;DM2PR0301MB0639;20:91u/auxB0IAY0lbZNiNFn/9zozVmkpExjwfDQNQ/XwYjVf1if8xuhulNg4zJ+WRYcHpbgwuDEudZP4p9Pld3dzQhCdztIZ+o6tppzjgO7ab+P/daotpRIcvZ84XwTz2EEEbkNZe+eDDoHnPeMVYT2Z/ko5JU/Qj4zDRYqAVBkUGMPC43NQ7gGaoljppxuWSIpni9KWEnD9HptjX8X5FRvpvj+Sk8yoe0HIT2NSo1Yz5ja6Xfme+VNbBNaJcLCHaLqhnX7n3hSRvqSHhovVA2Ym1ZMvcO8/dGkNZjwJseiS/9ZIo8C6lO78CsQGqPUtkmo4777+RvXoJGx6RVBSfZVYP9KbQQETedVZKU1oH6NkLGj++wMP6gqqeGD+1FBESL1Xv1NJmMfl9zfcUMSW8RcucIrBwW6upJoyfR81hUG6ndlu8mE+jLEJZ8552GabAbTAm6wBVwPjxaDhc3CDxJkCP2FKNhc7YwXMldEHpSmx7wMrBKIH4lrrPftucws6u1\r\nX-Exchange-Antispam-Report-Test: UriScan:;\r\nX-Exchange-Antispam-Report-CFA-Test: BCL:0;PCL:0;RULEID:(9101531078)(601004)(2401047)(13018025)(13016025)(8121501046)(13024025)(13023025)(3002001)(10201501046);SRVR:DM2PR0301MB0639;BCL:0;PCL:0;RULEID:;SRVR:DM2PR0301MB0639;\r\nX-Microsoft-Exchange-Diagnostics: 1;DM2PR0301MB0639;4:tNdaZ6aBrcavPGnvwqI5eE6ffo9oDKjLRTsuqr2ZC2bZjKa7La/VsUvCwOGyD4vHoV56geBvpfpbDi4KDUoxZ2P0NIYBuYYpdbRzKgZ2HRR2gIm0n90BpdE2+tX5+Z40+4A2+UrCzEIGgTvKsqTkZPkyGDNMIZ1BTKqGhRJp/acLjP4s+H+AFeX7XqqQnq5TFRsuNlbyteKgox74v2i3uK6UfrAKqPs22OGP+FVVa5v1q5FLj1zXRgdoxWyhx1QNGo+GLgklNtS9SJTc0qdn/lHesw7lnm/f8SbPP5bKAOB6qW1gEzrCP6bpN8MXPchL54lfmgf4XiF7rSLBFQs0JYgN1pYlUocI2d4YgVNdkgFCbxRUNGp+xG0frKoaHfe2lKs4UjZoLx6eH5HxeREVw668oZJzJ8qKLdF814DGyNYFMUPBO2mlG215nMxxb4Nf2wGkyCZoQ4YANR5gj6LiyA==\r\nX-MS-Exchange-Organization-SCL: 1\r\nX-Microsoft-Exchange-Diagnostics: =?us-ascii?Q?1;DM2PR0301MB0639;23:M9jzLLsl17ogj2hgBokJeRGoO9GXqVnenBxZYv6?=\r\n =?us-ascii?Q?cUtuupcAhhDfYw4kDIT3OgaIVFjQCUZQaZM1aTvSahQlLEKBAyggvb4iEt6G?=\r\n =?us-ascii?Q?puBvwiMciwCCQ0m01KG+CtbafwSncBIXf8RBAn3kA11WCo9GzlW0r9swNHE+?=\r\n =?us-ascii?Q?KObWx01aDKGtgf889sMDJATs48VaL9duIwZW0jCQjAN5gOgYUnjuupHOYYVi?=\r\n =?us-ascii?Q?mbCzHwdW4X+qdIFjKJqcopvssSJqWzi1cbw9ImCjL5IPOt0ySWdl8aia6aP0?=\r\n =?us-ascii?Q?mClbBRW7SLpC8hMeMp/lWEcoJ3t69qqIXi+Zi7qSkS3s41A+JQMpcm2H1waJ?=\r\n =?us-ascii?Q?UgLx/ll8CgalzrLHtVIOPDwPqh772fvDhKBl/fIhxnKfiRu5Yy1SMU2YQo5R?=\r\n =?us-ascii?Q?2627WlSfBjezgzsIy6AbCAo1oMUFeUfGzzty6q/EfKpD324JG/m1/1KoVsPU?=\r\n =?us-ascii?Q?hn8Ph7qZuWHKFS/HtpsvS0BuleaMs8wHaZuY5z2v7seq8C2WjDmU+r6qWivE?=\r\n =?us-ascii?Q?gGMMh88O6O/HmxIGJ5mjdzFzZz4no4Ph9mgGFfNVQZ7zx7o3lMLWt8v6stV6?=\r\n =?us-ascii?Q?fjswmQOiPAuu5RUcH6islmO/kCjqFQK5iNicLk0T/j/KQnInRu/p0mWvsQLc?=\r\n =?us-ascii?Q?Umm6d5/UBdfdiMRYRsNm1cgAm3rkJe8layH/soO6qhJN6fR/QCV2VZG/D0kP?=\r\n =?us-ascii?Q?eEbxLt/tJMj1tXxH5KaCxUeNP+wZCDxnPab2yowR4Af9Uhox5+GXQUbLJSBa?=\r\n =?us-ascii?Q?V5227YRQjDEGv9Z56YN1xnaxEAVgHZmD5iGwozDrRRN3eUgBsUUl4dpYef/r?=\r\n =?us-ascii?Q?bZ0z0EGgH1Zo/jjDaIZGjXfA+7lAk+tc8nadvvXSkUQbxMCYMQoV87XxMxZR?=\r\n =?us-ascii?Q?TKFasjTFVDJb6mUXy/XYymWjnZSjZcu9BNti/juu1geMHsFg0VXC9mtxvL01?=\r\n =?us-ascii?Q?gH6s9Jyrgt7TuK05vrV87mgKQcnrx2ZArginFY9qrsqnDLCuBgZ/mslZiKN2?=\r\n =?us-ascii?Q?LUtW5VUVhdOBPeqhnEZPjUyTu/b5ZiITGKCgubAihq6yb2Q1l/BirDacwwtT?=\r\n =?us-ascii?Q?UGqWodLnavyJQkHk2q6t4m9ePvPKBaCDHacu4VJYa+riwHA2mwWiQouqGn6o?=\r\n =?us-ascii?Q?5Far2/ana1UGFXZncCIe8VheTwusgg8qpS1sG+FPD8O/dkP64ttxYP0wRmjW?=\r\n =?us-ascii?Q?sPEkC++f2K5fjSWUYhpxjqIYdxUdD9sj5KERW8zrgfZP8NwSO2kcaTfYszXm?=\r\n =?us-ascii?Q?jguRHtf0wCp+xe6i/kPc7vOM8oYHAk8JeyTTYpdGyU+LEuogXmDqYUTt6B5X?=\r\n =?us-ascii?Q?1mg=3D=3D?=\r\nX-Microsoft-Exchange-Diagnostics: 1;DM2PR0301MB0639;6:jjQKqAiCUH9NndkP1LR31JRfpuz+H4+RtB1i8DlqI4TXEnIVIntWSLDPs3naRV6gVpNqtmjsTsdZmlRzru4SH6dceZYRB4/u2ePqKKgBDnVy/c4XR4i3LdgamtxDrwQc0UfcCdMC7aPHTrPmPWp5JAlEu/Mjxr8w6hroHLkO16YCKmOMkdN9SMcVh/ZdqZ0mGmpVo5J1yhMowh0Vc36KbLcZny1vanMQxuGahb6ukbxY3VNIhtCG0AX4Fy91ikGk/Sw2ywFWPoo7mYmw5DmqyM1ASML5xUQipkVeCvFLCEvF3wGJcsGTaPeHCVnbls6C;5:lSEc0yv7jeSL2W+Dho31gddSfYKugDvE+xQtEoiKnyoy5ji1F7tAhNvEeKfEmoF6jMKBJGoeiGyBSNXqQ0x+2m36VhyKc1mkqGE4xePluJF530lKr0dfbFmOaUiCZMnEztjdCwdv8+bsXdT7LF6JpQ==;24:77HdXsMAPk2sEINKlQ/Qr5TJ/yePc0MFUq7aTSKKpVTh3axeLtTyLE8cKiOJWq8oHNd88iiTUl0UN9n7TGrYKuKXO0ZK7PVeXJrOQC3TpaU=;7:ikhUDjhqAfMBzONJZTOdjcyQ7jigOlkvLV43x8gvK/EEmPaaFPdQDy7t9aZ27x98lpgghTAyBQQlisF0obDeflHsoAhuwbtHFpRMEEazGlOxOiTyM5OkoL6jEt8ukEuyyziwx2GM9HSgzAKGjPB/9C2H6ZJ+wG7KkhNsHlWZyfG0Q61ehY4Cgr7XqJBl1ZeRldxmFsciAaO7fuT7JEwpeGRgKZBrfvKtq8KeNxFlmqtxxw+syYt/WZjWt/CVtuXCTAyqbGvyDQs5UNH4uLwsqg==\r\nSpamDiagnosticOutput: 1:99\r\nSpamDiagnosticMetadata: NSPM\r\nX-MS-Exchange-CrossTenant-OriginalArrivalTime: 12 Jul 2016 20:38:13.4535\r\n (UTC)\r\nX-MS-Exchange-CrossTenant-Id: 52ec8a0a-dc28-4845-b421-6038854d679f\r\nX-MS-Exchange-CrossTenant-FromEntityHeader: Internet\r\nX-MS-Exchange-Transport-CrossTenantHeadersStamped: DM2PR0301MB0639\r\nX-MS-Exchange-Organization-AuthSource: BN1AFFO11FD037.protection.gbl\r\nX-MS-Exchange-Organization-AuthAs: Anonymous\r\nX-MS-Exchange-Transport-EndToEndLatency: 00:00:01.7934900\r\nX-Microsoft-Exchange-Diagnostics:\r\n\t1;BY2PR0301MB0632;9:iOOTSsUR7Cz2GPrKmmeqLorW5axNJfM6XrT85ypW8HIAu9syRxc+17/A3ijLSJ2XOf6f8VJNdYZzdxuOwsJkGuv48E3vaZpsXlodEqjOwV8sWcSwfmoln2SBYoEC9bhJm5YkUfSVk7i3qDZgyTkjjGB5M/MlqhcCGEUPWI0tV7/4nLxdNesqZpBzKqBg6Kac3L+8vh53zaZvyFBxHr4MSQ==\r\nContent-type: multipart/alternative;\r\n\tboundary=\"B_3551182709_1926180001\"\r\n\r\n> This message is in MIME format. Since your mail reader does not understand\r\nthis format, some or all of this message may not be legible.\r\n\r\n--B_3551182709_1926180001\r\nContent-type: text/plain;\r\n\tcharset=\"UTF-8\"\r\nContent-transfer-encoding: 7bit\r\n\r\nget 'em while they're hot.\r\n\r\n\r\n--B_3551182709_1926180001\r\nContent-type: text/html;\r\n\tcharset=\"UTF-8\"\r\nContent-transfer-encoding: quoted-printable\r\n\r\n<html>\r\n<head>\r\n<meta http-equiv=3D\"Content-Type\" content=3D\"text/html; charset=3Dutf-8\">\r\n</head>\r\n<body>\r\n<div dir=3D\"ltr\">get 'em while they're hot.</div>\r\n</body>\r\n</html>\r\n\r\n\r\n--B_3551182709_1926180001--\r\n\r\n".

decoded_mail() ->
 {<<"multipart">>,<<"mixed">>,
  [{<<"Return-Path">>,<<"<msfadmin@smtp.ms.scanlab.rapid7.com>">>},
   {<<"X-Original-To">>,<<"msfadmin@smtp.ms.scanlab.rapid7.com">>},
   {<<"Delivered-To">>,<<"msfadmin@smtp.ms.scanlab.rapid7.com">>},
   {<<"Received">>,
    <<"from [172.16.10.79] (unknown [172.16.10.79])by smtp.ms.scanlab.rapid7.com (Postfix) with ESMTPS id 6D72F1A0077for <msfadmin@smtp.ms.scanlab.rapid7.com>; Wed, 13 Jul 2016 09:33:44 -0700 (PDT)">>},
   {<<"From">>,<<"Sonny Gonzalez <msfadmin@smtp.ms.scanlab.rapid7.com>">>},
   {<<"Content-Type">>,
    <<"multipart/mixed; boundary=\"Apple-Mail=_5ACDB079-0CC2-4480-BDB7-2DEF977EE2F7\"">>},
   {<<"Subject">>,<<"test subject">>},
   {<<"Message-Id">>,
    <<"<3AA62083-629C-47CF-A87F-C2194FE04334@smtp.ms.scanlab.rapid7.com>">>},
   {<<"Date">>,<<"Wed, 13 Jul 2016 11:33:41 -0500">>},
   {<<"To">>,<<"Sonny Gonzalez <msfadmin@smtp.ms.scanlab.rapid7.com>">>},
   {<<"Mime-Version">>,<<"1.0 (Mac OS X Mail 9.3 \\(3124\\))">>},
   {<<"X-Mailer">>,<<"Apple Mail (2.3124)">>}],
  [{<<"content-type-params">>,
    [{<<"boundary">>,<<"Apple-Mail=_5ACDB079-0CC2-4480-BDB7-2DEF977EE2F7">>}]},
   {<<"disposition">>,<<"inline">>},
   {<<"disposition-params">>,[]}],
  [{<<"text">>,<<"plain">>,
    [{<<"Content-Transfer-Encoding">>,<<"7bit">>},
     {<<"Content-Type">>,<<"text/plain;charset=us-ascii">>}],
    [{<<"content-type-params">>,[{<<"charset">>,<<"us-ascii">>}]},
     {<<"disposition">>,<<"inline">>},
     {<<"disposition-params">>,[]}],
    <<"test body\r\n\r\n">>},
   {<<"message">>,<<"rfc822">>,
    [{<<"Content-Disposition">>,<<"attachment;filename=veggie-tails.eml">>},
     {<<"Content-Type">>,<<"message/rfc822;name=\"veggie-tails.eml\"">>},
     {<<"Content-Transfer-Encoding">>,<<"binary">>}],
    [{<<"content-type-params">>,[{<<"name">>,<<"veggie-tails.eml">>}]},
     {<<"disposition">>,<<"attachment">>},
     {<<"disposition-params">>,[{<<"filename">>,<<"veggie-tails.eml">>}]}],
    {<<"multipart">>,<<"alternative">>,
     [{<<"Received">>,
       <<"from DM2PR0301MB0639.namprd03.prod.outlook.com (10.160.96.13) byBY2PR0301MB0632.namprd03.prod.outlook.com (10.160.63.12) with Microsoft SMTPServer (version=TLS1_2, cipher=TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384_P384) id15.1.534.14 via Mailbox Transport; Tue, 12 Jul 2016 20:38:15 +0000">>},
      {<<"Received">>,
       <<"from BN3PR0301CA0064.namprd03.prod.outlook.com (10.160.152.160) byDM2PR0301MB0639.namprd03.prod.outlook.com (10.160.96.13) with Microsoft SMTPServer (version=TLS1_2, cipher=TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384_P384) id15.1.539.14; Tue, 12 Jul 2016 20:38:13 +0000">>},
      {<<"Received">>,
       <<"from BN1AFFO11FD037.protection.gbl (2a01:111:f400:7c10::172) byBN3PR0301CA0064.outlook.office365.com (2a01:111:e400:401e::32) with MicrosoftSMTP Server (version=TLS1_2,cipher=TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384_P384) id 15.1.539.14 viaFrontend Transport; Tue, 12 Jul 2016 20:38:13 +0000">>},
      {<<"Authentication-Results">>,
       <<"spf=pass (sender IP is 209.85.215.42)smtp.mailfrom=gmail.com; rapid7.com; dkim=pass (signature was verified)header.d=gmail.com;rapid7.com; dmarc=pass action=noneheader.from=gmail.com;rapid7.com; dkim=pass (signature was verified)header.d=gmail.com;">>},
      {<<"Received-SPF">>,
       <<"Pass (protection.outlook.com: domain of gmail.com designates209.85.215.42 as permitted sender) receiver=protection.outlook.com;client-ip=209.85.215.42; helo=mail-lf0-f42.google.com;">>},
      {<<"Received">>,
       <<"from mail-lf0-f42.google.com (209.85.215.42) byBN1AFFO11FD037.mail.protection.outlook.com (10.58.52.241) with Microsoft SMTPServer (TLS) id 15.1.523.9 via Frontend Transport; Tue, 12 Jul 2016 20:38:13+0000">>},
      {<<"Received">>,
       <<"by mail-lf0-f42.google.com with SMTP id q132so23247228lfe.3       for <sonny_gonzalez@rapid7.com>; Tue, 12 Jul 2016 13:38:13 -0700 (PDT)">>},
      {<<"DKIM-Signature">>,
       <<"v=1; a=rsa-sha256; c=relaxed/relaxed;       d=gmail.com; s=20120113;       h=mime-version:from:date:message-id:subject:to;       bh=D9DvOkwj75H75msH/FxMNg0fm28tsO8tE18iu+ZrNYc=;       b=KVtV8Kb+Ugy0llx8Ng3jw4N0vdx5QBN1kgM1s/Md1TPlLuLlabaENRdPNR0LXFsHCo        9JuUOKKmHZPZl33/K5iE0OLxT0NU0H+gMKgCrUrJYIPWr++rTKcne9wo0rj5nXgH2V2c        9wskv+JY033mVpC7g6jLDyD3x4iGxn5+6xgd6aKPGsZmmtlXrmJlMQXCHh++AU77OLS1        rI6M1T5JKE9VMiRyQuK7xZN8kp0U2jsywCniqsK5lFhcM8hzSz1x2DWzWcLVsOJ4FcrB        Uzsz1UJpcFjCgZfemMf0Iwk1yApphKDl1AI2FTDDDITI1CLb2y3VVMu/5jDTIMErIUDn        O/7A==">>},
      {<<"X-Google-DKIM-Signature">>,
       <<"v=1; a=rsa-sha256; c=relaxed/relaxed;       d=1e100.net; s=20130820;       h=x-gm-message-state:mime-version:from:date:message-id:subject:to;       bh=D9DvOkwj75H75msH/FxMNg0fm28tsO8tE18iu+ZrNYc=;       b=B7pdBoknrSYoRL7eDWhtOosnAmZCWLbnF5DEDqWUZEnGer++MmqU6J5m6I6ZGZ5PrV        20IQZcvK84OJfy7N8qLkZhtzIcvUBfWz/HUXtxpW4rL/qNmqdAw2KAtnjwZIKC/yjCMb        BsVpjrKdV8+CCze5rSQJ7qNs5IftKCllcMLhM0+/4twcXct2jMWt3UM+k5MlcelukPgu        NuXIav0z6H3B1Eq/0Fahk22lOX8Jm08aazBzwdnosBu2GPVrsM0/L6w4qay0ZhWther/        mtZfrTOq/xJ8alJj7DwstQNeMOYCKq3qvcxm5ejhFIr7iMjEgDltBnTys6C/nKFYyxY0        Ke7w==">>},
      {<<"X-Gm-Message-State">>,
       <<"ALyK8tJl9uDkpuRa8g41zLVK/jHdUcsurKH8LdDC0PsO9ogEYm3vUIYVnpQPvWG7JcBiVcxW61OGD9/JxHNI9g==">>},
      {<<"X-Received">>,
       <<"by 10.25.143.132 with SMTP id r126mr2193580lfd.160.1468355891628;Tue, 12 Jul 2016 13:38:11 -0700 (PDT)">>},
      {<<"MIME-Version">>,<<"1.0">>},
      {<<"Received">>,
       <<"by 10.25.216.6 with HTTP; Tue, 12 Jul 2016 13:38:11 -0700 (PDT)">>},
      {<<"From">>,<<"Sonny Gonzalez <sonnyg.email@gmail.com>">>},
      {<<"Date">>,<<"Tue, 12 Jul 2016 15:38:11 -0500">>},
      {<<"Message-ID">>,
       <<"<CA+9Poanvz6kYKJEw1GgnUXo2VD6djabR7uFNVnPRr9=ndHhQzQ@mail.gmail.com>">>},
      {<<"Subject">>,<<"veggie tails">>},
      {<<"To">>,<<"sonny_gonzalez@rapid7.com">>},
      {<<"Return-Path">>,<<"sonnyg.email@gmail.com">>},
      {<<"X-MS-Exchange-Organization-Network-Message-Id">>,
       <<"a50bcf85-9189-4b2b-bd5b-08d3aa94720c">>},
      {<<"X-EOPAttributedMessage">>,<<"0">>},
      {<<"X-EOPTenantAttributedMessage">>,
       <<"52ec8a0a-dc28-4845-b421-6038854d679f:0">>},
      {<<"X-MS-Exchange-Organization-MessageDirectionality">>,<<"Incoming">>},
      {<<"X-Forefront-Antispam-Report">>,
       <<"CIP:209.85.215.42;IPV:NLI;CTRY:US;EFV:NLI;SFV:NSPM;SFS:(6009001)(8156002)(2980300002)(438002)(189002)(199003)(93516999)(1096003)(98316002)(73972006)(3480700004)(59536001)(73392002)(50986999)(8896002)(82202001)(81442002)(61266001)(8676002)(92566002)(221733001)(11100500001)(450100001)(110136002)(956001)(107886002)(7846002)(54356999)(63696999)(189998001)(83322999)(8576002)(9896002)(6806005)(55446002)(2361001)(356003)(42186005)(305945005)(7596002)(2351001)(106466001)(84326002)(76482005)(564344004)(7636002)(87572001)(586003)(86362001)(512874002)(5000100001)(229853001)(246002)(16796002)(358055004);DIR:INB;SFP:;SCL:1;SRVR:DM2PR0301MB0639;H:mail-lf0-f42.google.com;FPR:;SPF:Pass;PTR:mail-lf0-f42.google.com;MX:1;A:1;LANG:en;">>},
      {<<"X-Microsoft-Exchange-Diagnostics">>,
       <<"1;BN1AFFO11FD037;1:UqfXGeDUAYVRAdJBO8tCfSEhgS2B10WkNiPiE9i1JtNi+/9ZeCjHn7iiYuLI+n9/zzHXkAuC9o9tZp93CKPnj7ZspAa0T2Am5Fp12lIoTtHnAnONeEHhXdZaYXby92MsKRc4gXsdYMOrIhk6zX3Kf5vU2upiPbwO9cn5mbWahG/GXkl9WAyxowjvdhGAje/Bvb2jO0RgecloW2TazLY786VGDH4UadBWo0yLh+LMbr92oAOoMwJLYn28sDQT0MkexQaZo7Qn1O09qjoN1hoJrqynSVrLzjB4FHVl4A3+IA4aGvBepWmS5YQ6qzBlKYOqbRD+T11BOmjDAM3VFepdIsqyRpirAkfkQlY50X4Q5IhK093b2gjb8piHNf/IpKjOrAG7FQikTVZ+5hpSHxhfiUl/WGWl1cjSDougsL2o0O4Qjsxl6bw211HkIYFfvwLDzPMozCX3ZJNYJ1msZKHYekQgi/UoTZZtG4SGXfiAaZSZot/sh0EE//dI72FBJZV+imZ9Te3xutQs/FU5nldSa+l2+pFWX1Y3K69BcIr9jVwwG0kALcV93GDxSKJNaqoP">>},
      {<<"X-MS-Office365-Filtering-Correlation-Id">>,
       <<"a50bcf85-9189-4b2b-bd5b-08d3aa94720c">>},
      {<<"X-Microsoft-Exchange-Diagnostics">>,
       <<"1;DM2PR0301MB0639;2:Ta63v9xsMa+iZcHPYseoXex8bIKyCN8JzzpMSZOweQy6LF3+w4DaGYAG7pAvEuCho5fCS2AUTLAE9YjCE6S0r/dPVZg49v8GTI17HoM7ONM5LE/U3h82wzDUVoSZBVAKenufqDPEF+4yRU+pt5e1Y9JbmZzHRU9WKQGa4gs1fLfKYbDaum4nPF+867MU7ev/;3:CGJtLCvN5Z5LJ2rCS9psQC3p266HOyPDlrYzkW34avjqB+LrF1ZAT4ZPlT9kc1nzhHqadGqH7HU3TC1Qcin3w3/qZyXinDnWl5jMS+1dM6u6vfLNg6buIXc/93Q3lDCt+xXTdgalYg/CyIS9bpBn9bq4S/SdV/JMVffNo2hlBOSgqREwsdxlu5Yp7sdNkVA/rqG2B/4TXDfGpsoDvLoQ0p/2WVtKymB3X7gww0pbdfjcefk1jBQS24lNENTFmzsqmaKzAdsoqr9CdfFIlaZoFqwI8RR2ZNajnveirzTD2p3HyBU/jX6jimrdEwTC8wpBBDdYhtbT9wHwFx1bd0UA6sCUhCCJG0n5sxCqQOhx1F8=">>},
      {<<"X-DkimResult-Test">>,<<"Passed">>},
      {<<"X-Microsoft-Antispam">>,
       <<"UriScan:;BCL:0;PCL:0;RULEID:(8251501002)(3001016)(3010002)(71701004)(71702002);SRVR:DM2PR0301MB0639;">>},
      {<<"X-Microsoft-Exchange-Diagnostics">>,
       <<"1;DM2PR0301MB0639;25:HAglOUBK4u8C+DKft1bi6kqirc7Cml1dor04o23Yu7sAgHCTU4lmGZAqCRbuOqXPqZL7hix3WyuWfp130omM52d0GK/S9F7dIC9qdxPHuv3brbQQ1uqi1v7DbrdbL4QYBh35Uns396jkWqwc0lZS0On4w0njg98vseBAPqxhK4ydHDfCg29Z2/1FLnYKm2sY4UZmTiQ8Wdev8O/epwGxI/K41ReUgCRcF0SjlTwYfLLf73NgRT0VOnN3Ks5VhOE6JKKTpN0JnYFdayrd7ZqIvQTe2xL4Q+wNk/8hSc+DxRk1na8y6szt6x82jcZmbJeWI0z+YJ1T94DlLUN4Kda+VQdhQ9XEjdcu13GGUmfcQudk5PGoPkf3ibhLypXtCz6Al4MRYdNiLTrOcSNNl3R+aIlKgJvFJtpr8GYxEDVvWOY=;31:k/j1CwQq+DtMgUwyr+TFfFcyOrrBJHW/5qBfXGoTRk4vcNtvWOd0jpOnEMdHamLYRa8qVwTsbcAbb3hm3WxbrU8eVHwTi0w6bwCcXWYV1N+rFNWDOFck1ZvmqT/hVn2GwmrYCLNP9LqlKbwMkJ0NPW3XYHEyl3nFZFe7RDILC233VqxAll7nJXV5Dd5BC51uMubicW6exkwM6PBT7eM7ww==">>},
      {<<"X-MS-Exchange-Organization-AVStamp-Service">>,<<"1.0">>},
      {<<"X-Microsoft-Exchange-Diagnostics">>,
       <<"1;DM2PR0301MB0639;20:91u/auxB0IAY0lbZNiNFn/9zozVmkpExjwfDQNQ/XwYjVf1if8xuhulNg4zJ+WRYcHpbgwuDEudZP4p9Pld3dzQhCdztIZ+o6tppzjgO7ab+P/daotpRIcvZ84XwTz2EEEbkNZe+eDDoHnPeMVYT2Z/ko5JU/Qj4zDRYqAVBkUGMPC43NQ7gGaoljppxuWSIpni9KWEnD9HptjX8X5FRvpvj+Sk8yoe0HIT2NSo1Yz5ja6Xfme+VNbBNaJcLCHaLqhnX7n3hSRvqSHhovVA2Ym1ZMvcO8/dGkNZjwJseiS/9ZIo8C6lO78CsQGqPUtkmo4777+RvXoJGx6RVBSfZVYP9KbQQETedVZKU1oH6NkLGj++wMP6gqqeGD+1FBESL1Xv1NJmMfl9zfcUMSW8RcucIrBwW6upJoyfR81hUG6ndlu8mE+jLEJZ8552GabAbTAm6wBVwPjxaDhc3CDxJkCP2FKNhc7YwXMldEHpSmx7wMrBKIH4lrrPftucws6u1">>},
      {<<"X-Exchange-Antispam-Report-Test">>,<<"UriScan:;">>},
      {<<"X-Exchange-Antispam-Report-CFA-Test">>,
       <<"BCL:0;PCL:0;RULEID:(9101531078)(601004)(2401047)(13018025)(13016025)(8121501046)(13024025)(13023025)(3002001)(10201501046);SRVR:DM2PR0301MB0639;BCL:0;PCL:0;RULEID:;SRVR:DM2PR0301MB0639;">>},
      {<<"X-Microsoft-Exchange-Diagnostics">>,
       <<"1;DM2PR0301MB0639;4:tNdaZ6aBrcavPGnvwqI5eE6ffo9oDKjLRTsuqr2ZC2bZjKa7La/VsUvCwOGyD4vHoV56geBvpfpbDi4KDUoxZ2P0NIYBuYYpdbRzKgZ2HRR2gIm0n90BpdE2+tX5+Z40+4A2+UrCzEIGgTvKsqTkZPkyGDNMIZ1BTKqGhRJp/acLjP4s+H+AFeX7XqqQnq5TFRsuNlbyteKgox74v2i3uK6UfrAKqPs22OGP+FVVa5v1q5FLj1zXRgdoxWyhx1QNGo+GLgklNtS9SJTc0qdn/lHesw7lnm/f8SbPP5bKAOB6qW1gEzrCP6bpN8MXPchL54lfmgf4XiF7rSLBFQs0JYgN1pYlUocI2d4YgVNdkgFCbxRUNGp+xG0frKoaHfe2lKs4UjZoLx6eH5HxeREVw668oZJzJ8qKLdF814DGyNYFMUPBO2mlG215nMxxb4Nf2wGkyCZoQ4YANR5gj6LiyA==">>},
      {<<"X-MS-Exchange-Organization-SCL">>,<<"1">>},
      {<<"X-Microsoft-Exchange-Diagnostics">>,
       <<"1;DM2PR0301MB0639;23:M9jzLLsl17ogj2hgBokJeRGoO9GXqVnenBxZYv6cUtuupcAhhDfYw4kDIT3OgaIVFjQCUZQaZM1aTvSahQlLEKBAyggvb4iEt6GpuBvwiMciwCCQ0m01KG+CtbafwSncBIXf8RBAn3kA11WCo9GzlW0r9swNHE+KObWx01aDKGtgf889sMDJATs48VaL9duIwZW0jCQjAN5gOgYUnjuupHOYYVimbCzHwdW4X+qdIFjKJqcopvssSJqWzi1cbw9ImCjL5IPOt0ySWdl8aia6aP0mClbBRW7SLpC8hMeMp/lWEcoJ3t69qqIXi+Zi7qSkS3s41A+JQMpcm2H1waJUgLx/ll8CgalzrLHtVIOPDwPqh772fvDhKBl/fIhxnKfiRu5Yy1SMU2YQo5R2627WlSfBjezgzsIy6AbCAo1oMUFeUfGzzty6q/EfKpD324JG/m1/1KoVsPUhn8Ph7qZuWHKFS/HtpsvS0BuleaMs8wHaZuY5z2v7seq8C2WjDmU+r6qWivEgGMMh88O6O/HmxIGJ5mjdzFzZz4no4Ph9mgGFfNVQZ7zx7o3lMLWt8v6stV6fjswmQOiPAuu5RUcH6islmO/kCjqFQK5iNicLk0T/j/KQnInRu/p0mWvsQLcUmm6d5/UBdfdiMRYRsNm1cgAm3rkJe8layH/soO6qhJN6fR/QCV2VZG/D0kPeEbxLt/tJMj1tXxH5KaCxUeNP+wZCDxnPab2yowR4Af9Uhox5+GXQUbLJSBaV5227YRQjDEGv9Z56YN1xnaxEAVgHZmD5iGwozDrRRN3eUgBsUUl4dpYef/rbZ0z0EGgH1Zo/jjDaIZGjXfA+7lAk+tc8nadvvXSkUQbxMCYMQoV87XxMxZRTKFasjTFVDJb6mUXy/XYymWjnZSjZcu9BNti/juu1geMHsFg0VXC9mtxvL01gH6s9Jyrgt7TuK05vrV87mgKQcnrx2ZArginFY9qrsqnDLCuBgZ/mslZiKN2LUtW5VUVhdOBPeqhnEZPjUyTu/b5ZiITGKCgubAihq6yb2Q1l/BirDacwwtTUGqWodLnavyJQkHk2q6t4m9ePvPKBaCDHacu4VJYa+riwHA2mwWiQouqGn6o5Far2/ana1UGFXZncCIe8VheTwusgg8qpS1sG+FPD8O/dkP64ttxYP0wRmjWsPEkC++f2K5fjSWUYhpxjqIYdxUdD9sj5KERW8zrgfZP8NwSO2kcaTfYszXmjguRHtf0wCp+xe6i/kPc7vOM8oYHAk8JeyTTYpdGyU+LEuogXmDqYUTt6B5X1mg==">>},
      {<<"X-Microsoft-Exchange-Diagnostics">>,
       <<"1;DM2PR0301MB0639;6:jjQKqAiCUH9NndkP1LR31JRfpuz+H4+RtB1i8DlqI4TXEnIVIntWSLDPs3naRV6gVpNqtmjsTsdZmlRzru4SH6dceZYRB4/u2ePqKKgBDnVy/c4XR4i3LdgamtxDrwQc0UfcCdMC7aPHTrPmPWp5JAlEu/Mjxr8w6hroHLkO16YCKmOMkdN9SMcVh/ZdqZ0mGmpVo5J1yhMowh0Vc36KbLcZny1vanMQxuGahb6ukbxY3VNIhtCG0AX4Fy91ikGk/Sw2ywFWPoo7mYmw5DmqyM1ASML5xUQipkVeCvFLCEvF3wGJcsGTaPeHCVnbls6C;5:lSEc0yv7jeSL2W+Dho31gddSfYKugDvE+xQtEoiKnyoy5ji1F7tAhNvEeKfEmoF6jMKBJGoeiGyBSNXqQ0x+2m36VhyKc1mkqGE4xePluJF530lKr0dfbFmOaUiCZMnEztjdCwdv8+bsXdT7LF6JpQ==;24:77HdXsMAPk2sEINKlQ/Qr5TJ/yePc0MFUq7aTSKKpVTh3axeLtTyLE8cKiOJWq8oHNd88iiTUl0UN9n7TGrYKuKXO0ZK7PVeXJrOQC3TpaU=;7:ikhUDjhqAfMBzONJZTOdjcyQ7jigOlkvLV43x8gvK/EEmPaaFPdQDy7t9aZ27x98lpgghTAyBQQlisF0obDeflHsoAhuwbtHFpRMEEazGlOxOiTyM5OkoL6jEt8ukEuyyziwx2GM9HSgzAKGjPB/9C2H6ZJ+wG7KkhNsHlWZyfG0Q61ehY4Cgr7XqJBl1ZeRldxmFsciAaO7fuT7JEwpeGRgKZBrfvKtq8KeNxFlmqtxxw+syYt/WZjWt/CVtuXCTAyqbGvyDQs5UNH4uLwsqg==">>},
      {<<"SpamDiagnosticOutput">>,<<"1:99">>},
      {<<"SpamDiagnosticMetadata">>,<<"NSPM">>},
      {<<"X-MS-Exchange-CrossTenant-OriginalArrivalTime">>,
       <<"12 Jul 2016 20:38:13.4535(UTC)">>},
      {<<"X-MS-Exchange-CrossTenant-Id">>,
       <<"52ec8a0a-dc28-4845-b421-6038854d679f">>},
      {<<"X-MS-Exchange-CrossTenant-FromEntityHeader">>,<<"Internet">>},
      {<<"X-MS-Exchange-Transport-CrossTenantHeadersStamped">>,
       <<"DM2PR0301MB0639">>},
      {<<"X-MS-Exchange-Organization-AuthSource">>,
       <<"BN1AFFO11FD037.protection.gbl">>},
      {<<"X-MS-Exchange-Organization-AuthAs">>,<<"Anonymous">>},
      {<<"X-MS-Exchange-Transport-EndToEndLatency">>,<<"00:00:01.7934900">>},
      {<<"X-Microsoft-Exchange-Diagnostics">>,
       <<"1;BY2PR0301MB0632;9:iOOTSsUR7Cz2GPrKmmeqLorW5axNJfM6XrT85ypW8HIAu9syRxc+17/A3ijLSJ2XOf6f8VJNdYZzdxuOwsJkGuv48E3vaZpsXlodEqjOwV8sWcSwfmoln2SBYoEC9bhJm5YkUfSVk7i3qDZgyTkjjGB5M/MlqhcCGEUPWI0tV7/4nLxdNesqZpBzKqBg6Kac3L+8vh53zaZvyFBxHr4MSQ==">>},
      {<<"Content-type">>,
       <<"multipart/alternative;boundary=\"B_3551182709_1926180001\"">>}],
     [{<<"content-type-params">>,
       [{<<"boundary">>,<<"B_3551182709_1926180001">>}]},
      {<<"disposition">>,<<"inline">>},
      {<<"disposition-params">>,[]}],
     [{<<"text">>,<<"plain">>,
       [{<<"Content-type">>,<<"text/plain;charset=\"UTF-8\"">>},
        {<<"Content-transfer-encoding">>,<<"7bit">>}],
       [{<<"content-type-params">>,[{<<"charset">>,<<"UTF-8">>}]},
        {<<"disposition">>,<<"inline">>},
        {<<"disposition-params">>,[]}],
       <<"get 'em while they're hot.\r\n\r\n">>},
      {<<"text">>,<<"html">>,
       [{<<"Content-type">>,<<"text/html;charset=\"UTF-8\"">>},
        {<<"Content-transfer-encoding">>,<<"quoted-printable">>}],
       [{<<"content-type-params">>,[{<<"charset">>,<<"UTF-8">>}]},
        {<<"disposition">>,<<"inline">>},
        {<<"disposition-params">>,[]}],
       <<"<html>\r\n<head>\r\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">\r\n</head>\r\n<body>\r\n<div dir=\"ltr\">get 'em while they're hot.</div>\r\n</body>\r\n</html>\r\n\r\n">>}]}}]}.