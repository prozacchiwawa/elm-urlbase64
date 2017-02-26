module UrlBase64 exposing (encode, decode)

{-|

A couple of functions for use with a base64 encoder and decoder that convert the
base64 alphabet to and from the url alphabet.

They can be composed with encode and decode in truqu/elm-base64 like this:

    b64e = UrlBase64.encode Base64.encode
    b64d = UrlBase64.decode Base64.decode

Applying these to url base64 converts to and from standard base64 into and out
of the decoders underneath.

    base64_1 = b64e "a\255\255" -- Ok "Yf__"
    base64_t = b64e "a\255\255" |> Result.andThen b64d -- Ok "aÿÿ"
    base64_2 = b64e "a\255" -- Ok "Yf8"
    base64_u = b64e "a\255" |> Result.andThen b64d -- Ok "aÿ"

@docs encode, decode

-}

import Regex exposing (Regex)

replaceForUrl : Regex
replaceForUrl = Regex.regex "[\\+/=]"

{-| Expose the given function to the given string and convert the result from
the standard base64 alphabet and trim trailing '=' characters.

Compose this with a base64 encoder to make a url-base64 encoder.

    b64e = UrlBase64.encode Base64.encode

-}
encode : (a -> Result String String) -> a -> Result String String
encode enc t =
    let replaceChar {match} =
        case match of
            "+" -> "-"
            "/" -> "_"
            _ -> ""
    in
    enc t
    |> Result.map (Regex.replace Regex.All replaceForUrl replaceChar)

replaceFromUrl : Regex
replaceFromUrl = Regex.regex "[-_]"

{-|
Expose the given function to the standard base64 alphabet form of the given
string with padding restored.

Compose this with a base64 decoder to make a url-base64 decoder.

    b64d = UrlBase64.decode Base64.decode

-}
decode : (String -> Result String a) -> String -> Result String a
decode dec e =
    let replaceChar {match} =
        case match of
            "-" -> "+"
            _ -> "/"
    in
    let ilen = (4 - (String.length e)) % 4 in
    dec (Regex.replace Regex.All replaceFromUrl replaceChar (e ++ (String.repeat ilen "=")))
