-ifdef(OTP_RELEASE).
    -define(
        define_stacktrace(Type, Reason, Stacktrace), Type:Reason:Stacktrace).
    -define(get_stacktrace(Stacktrace), Stacktrace).
-else.
    -define(
        define_stacktrace(Type, Reason, Stacktrace),
        Type:Reason
    ).
    -define(get_stacktrace(Stacktrace), erlang:get_stacktrace()).
-endif.
