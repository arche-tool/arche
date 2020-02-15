let Json : Type = Text
let S3Location : Type = Text
let EndpointConfiguration : Type = Text
let Tag : Type = Text
let KV : Type = Text

let RestApiSharedProperties : Type = {
    ApiKeySourceType : Text,
    BinaryMediaTypes : List Text,
    CloneFrom : Optional Text,
    Description : Text,
    EndpointConfiguration : EndpointConfiguration,
    FailOnWarnings : Bool,
    MinimumCompressionSize : Natural,
    Name : Text,
    Parameters : List (KV),
    Policy : Json,
    Tags : List Tag
}

let Properties
    = < A : RestApiSharedProperties //\\ {Body : Json}
    | B : RestApiSharedProperties //\\ {BodyS3Location : S3Location}
    >

let RestApi : Type = {
  Type : Text,
  Properties : Properties
}

let restApi : RestApi = {
    Type = "AWS::ApiGateway::RestApi",
    Properties = Properties.A ({
      ApiKeySourceType = "",
      BinaryMediaTypes = ["text/plain"],
      CloneFrom = None Text,
      Description = "",
      EndpointConfiguration = "",
      FailOnWarnings = False,
      MinimumCompressionSize = 100,
      Name = "",
      Parameters = [""],
      Policy = "",
      Tags = ["arche"]
    } // {Body = "asdf"})
}
in restApi