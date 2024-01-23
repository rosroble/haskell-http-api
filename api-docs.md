## GET /docs

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `text/plain;charset=utf-8`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
""
```

- Example (`text/plain;charset=utf-8`):

```

```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
"a"
```

## GET /get

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/xml;charset=utf-8`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"keyToGet":"key_to_lookup"}
```

- Example (`application/xml;charset=utf-8`):

```xml
<GetRequest><Key>key_to_lookup</Key></GetRequest>
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"key":"Alex","value":22}
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"key":1024,"value":"abc"}
```

- Example (`application/json;charset=utf-8`):

```javascript
{"key":[1,"str"],"value":1002}
```

## GET /lists/len

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/xml;charset=utf-8`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"name":"listname"}
```

- Example (`application/xml;charset=utf-8`):

```xml
<ListLenRequest><Name>listname</Name></ListLenRequest>
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"len":15,"success":true}
```

## POST /lists/lpop

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/xml;charset=utf-8`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"name":"somelist"}
```

- Example (`application/xml;charset=utf-8`):

```xml
<ListPopRequest><Name>somelist</Name></ListPopRequest>
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"error":"","success":true}
```

## POST /lists/lpush

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/xml;charset=utf-8`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"name":"some_str_key","value":3}
```

- Example (`application/xml;charset=utf-8`):

```xml
<ListPushRequest><Name>some_str_key</Name><Value>3</Value></ListPushRequest>
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"error":"error","success":false}
```

## POST /lists/rpop

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/xml;charset=utf-8`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"name":"somelist"}
```

- Example (`application/xml;charset=utf-8`):

```xml
<ListPopRequest><Name>somelist</Name></ListPopRequest>
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"error":"","success":true}
```

## POST /lists/rpush

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/xml;charset=utf-8`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"name":"some_str_key","value":3}
```

- Example (`application/xml;charset=utf-8`):

```xml
<ListPushRequest><Name>some_str_key</Name><Value>3</Value></ListPushRequest>
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"error":"error","success":false}
```

## POST /lists/trim

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/xml;charset=utf-8`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"name":"list","start":1,"stop":5}
```

- Example (`application/xml;charset=utf-8`):

```xml
<ListPushRequest><Name>list</Name><Start>1</Start><Stop>5</Stop></ListPushRequest>
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"error":"","success":true}
```

## POST /set

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/xml;charset=utf-8`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"key":"Alex","value":22}
```

- Example (`application/xml;charset=utf-8`):

```xml
<KVEntry><Key>Alex</Key><Value>22</Value></KVEntry>
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"key":1024,"value":"abc"}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript

```

## GET /strings/get

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/xml;charset=utf-8`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"stringKey":"some_str_key"}
```

- Example (`application/xml;charset=utf-8`):

```xml
<StringGetRequest><StringKey>some_str_key</StringKey></StringGetRequest>
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"success":true,"value":"some_str_key"}
```

## GET /strings/mget

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/xml;charset=utf-8`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"stringKeys":["some_str_key","another_str_key"]}
```

- Example (`application/xml;charset=utf-8`):

```xml
<StringMGetRequest>some_str_keyanother_str_key</StringMGetRequest>
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"results":[{"success":true,"value":"some_str_key"},{"success":true,"value":"another_str_key"}]}
```

## POST /strings/set

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/xml;charset=utf-8`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"stringKey":"some_str_key","stringValue":"some_str_val"}
```

- Example (`application/xml;charset=utf-8`):

```xml
<StringSetRequest><StringKey>some_str_key</StringKey><StringValue>some_str_val</StringValue></StringSetRequest>
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript

```

## POST /strings/setnx

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/xml;charset=utf-8`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"stringKey":"some_str_key","stringValue":"some_str_val"}
```

- Example (`application/xml;charset=utf-8`):

```xml
<StringSetRequest><StringKey>some_str_key</StringKey><StringValue>some_str_val</StringValue></StringSetRequest>
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"success":true}
```

