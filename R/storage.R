#' storage for unikonstanzmensabot infos
storage <- new.env()
storage$tables <- list()

storage$tables$dishes <-
  'CREATE TABLE dishes (
    "loc"  TEXT,
    "date" TEXT,
    "lang" TEXT,
    "type" TEXT,
    "dish" TEXT,
    "additives" TEXT,
    "http_status" INTEGER,
    "content_length" INTEGER
    )'

storage$tables$requests <-
  'CREATE TABLE requests (
        "post_date" TEXT,
        "status" INTEGER,
        "func" TEXT,
        "loc" TEXT,
        "lang" TEXT,
        "date" TEXT,
        "t_redirect" REAL,
        "t_namelookup" REAL,
        "t_connect" REAL,
        "t_pretransfer" REAL,
        "t_starttransfer" REAL,
        "t_total" REAL,
        "cookies_domain" TEXT,
        "cookies_flag" TEXT,
        "cookies_path" TEXT,
        "cookies_secure" TEXT,
        "cookies_expiration" TEXT,
        "cookies_name" TEXT,
        "cookies_value" TEXT,
        "content" TEXT
      )'

storage$tables$requests <-
  'CREATE TABLE tweets (
    "location" TEXT,
    "date_dish" TEXT,
    "language" TEXT,
    "types" TEXT,
    "attempts" INTEGER,
    "tweeted" INTEGER,
    "date_attempts" INTEGER,
    "tweet" TEXT
    )'
