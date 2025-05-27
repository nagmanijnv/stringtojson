# stringtojson
This api converts a string to json. But string should be in particular format otherwise
it will throw an error mentioning error reason.

## curl for the API

```bash
curl --location 'http://localhost:8081/stringtojson' \
--header 'Content-Type: application/json' \
--data '{
    "encodedString": "#00date|1997-02-06#02name|bob#01age|20#13hasPassport|Y,y,t#12access|read_db,write_db"
}'
```

- Different Input
```
"#00date|1997-02-06#02name|bob#01age|20#03hasPassport|Y#12access|read_db,write_db,view_logs"
"#01date|1997-02-06#02name|bob#01age|20#03hasPassport|Y#12access|read_db,write_db"
```



## Run application
```
cabal run
```