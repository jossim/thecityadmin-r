TheCity <- setClass("TheCity",
                    slots = list(secret_key = "character", 
                                 user_token = "character",
                                 users = "data.frame",
                                 groups = "data.frame",
                                 campuses = "data.frame",
                                 rate_limit = "numeric",
                                 rate_limit_remaing = "numeric",
                                 max_sleep_time = "numeric")
                    )


setValidity("TheCity", function(object) {
    msg = NULL
    valid = TRUE
    
    if (!nchar(object@secret_key) > 0) {
        valid = FALSE
        msg = c(msg, "Must have a secret key")
    }

    if (!nchar(object@user_token) > 0) {
        valid = FALSE
        msg = c(msg, "Must have a user token")
    }
})