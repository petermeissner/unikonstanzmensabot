#' storage for unikonstanzmensabot infos
storage <- new.env()
storage$db_path <- ""
storage$tables <- list()

storage$tables$dishes <-
  'CREATE TABLE dishes (
    "loc"  TEXT,
    "lang" TEXT,
    "date" TEXT,
    "type" TEXT,
    "dish" TEXT,
    "additives" TEXT,
    PRIMARY KEY (loc, lang, date, type)
    )'

storage$tables$requests <-
  'CREATE TABLE requests (
        "req_date" TEXT,
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
        "length" INTEGER,
        "content" TEXT
      )'

storage$tables$tweets <-
  'CREATE TABLE tweets (
    "loc" TEXT,
    "lang" TEXT,
    "date" TEXT,
    "type" TEXT,
    "id_str" INTEGER,
    "tweet" INTEGER
  )'

storage$additives <-
rbind(
  c(1 , "Farbstoff"),
  c(2 , "Konservierungsstoff"),
  c(3 , "Antioxidationsmittel"),
  c(4 , "Geschmacksverstärker"),
  c(5 , "geschwefelt"),
  c(6 , "geschwärzt"),
  c(7 , "gewachst"),
  c(8 , "Phosphat"),
  c(9 , "Süßungsmittel"),
  c(10, "enthält eine Phenylalaninquelle"),
  c(11, "genetisch verändert"),
  c(12, "enthält Sojaöl aus genetisch verändertem Soja hergestellt"),
  c("Sch", "Schweinefleisch"),
  c("R" , "Rindfleisch"),
  c("Veg", "Vegetarische Kost"),
  c("L" , "Lamm"),
  c("K" , "Kalb"),
  c("F" , "Fisch"),
  c("G" , "Geflügel"),
  c(25, "glutenhaltiges Getreide"),
  c(26, "Fisch"),
  c(27, "Krebstiere"),
  c(28, "Eier"),
  c(29, "Erdnüsse"),
  c(30, "Soja"),
  c(31, "Milch (einschließlich Laktose)"),
  c(32, "Schalenfrüchte"),
  c(33, "Sellerie"),
  c(34, "Senf"),
  c(35, "Sesam"),
  c(36, "Schwefeldioxid bzw. Sulfite ab 10mg pro kg/ltr"),
  c(37, "Lupine"),
  c(38, "Weichtiere, und Erzeugnisse aus 25-38")
)