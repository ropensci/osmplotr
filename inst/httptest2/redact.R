function (resp) {

    resp <- httptest2::gsub_response (
        resp,
        "overpass.*api\\/",
        "overpass/",
        fixed = FALSE
    )

    # Timestamp pattern:
    ptn <- paste0 (
        "[A-Za-z]{3},\\s[0-9]{2}\\s[A-Za-z]{3}\\s[0-9]{4}\\s", # date
        "[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}"
    ) # time
    resp <- httptest2::gsub_response (
        resp,
        ptn,
        "Sat, 01 Jan 20222 00:00:00",
        fixed = FALSE
    )

    # Datestamp pattern (for api status requests):
    ptn <- "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}T[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}Z"
    resp <- httptest2::gsub_response (
        resp,
        ptn,
        "2022-01-01T00:00:00Z",
        fixed = FALSE
    )
    ptn <- "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}"
    resp <- httptest2::gsub_response (
        resp,
        ptn,
        "2022-01-01",
        fixed = FALSE
    )

    # overpass status with encoded ip address:
    resp <- httptest2::gsub_response (
        resp,
        "Connected\\sas\\:\\s[0-9]*",
        "Connected as: 123456789",
        fixed = FALSE
    )

    return (resp)
}
