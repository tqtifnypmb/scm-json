scm-json is a json parser/builder written in scheme. It's almost compatible to [JSON](http://www.json.org/)(unicode string value isn't considered).


#### Overview

json objects are represented using scheme's native object as following:

    array  -->  list
  
    dictionary --> list of pair
  
    string -->  string
  
    null   -->  '()
  
    number -->  number
  
    bool   -->  boolean
  
#### Usage

- string->json

  (string->json `"[true, false]") `   
  `--> (#t #f)`
  
  (string->json `"{\"key\": \"value\"}") `   
  `--> (("key" . "value"))`
  
- json->string

  (json->string `'(#t #f))`      
  `--> "[true, false]"`
  
  (json->string `` `(,(cons "key" "value")))``  
  `--> {"key" : "value"}`
