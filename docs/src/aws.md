### Using AWS to host Arche

Arche is deployed on cloud using AWS services. Bellow a quick description of main services and their configurations.

## AWS Lambda

# JSON endpoints
Let's delegate to the lambda function to parse and check the format of the json body. This can be achieved by setting the mapping template (associated content-type:application/json) with code bellow and making sure the content-type is NOT set as binary media type.
```
{
    ...
    "body": $input.json('$')
    ...
}
```

# ANG upload
In the case of ANG files the content-type used is text/plain. Due it's expected file size and format, the this content-type is set as binary media type and AWS Gateway will take care of encoding the body in base64. And since the body isn't a json anymore, the mapping template is the set to: 
```
{
    ...
    "body": "$input.bod"
    ...
}
```

