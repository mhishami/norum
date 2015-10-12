

%% record to keep the key, value
-record(norum_session, {key, val, expiry, timestamp = {date(), time()}}).
-record(norum_cookies, {key, val}).
