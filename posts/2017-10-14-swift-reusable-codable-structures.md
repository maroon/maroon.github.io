---
title: "Swift 4 Reusable Codable Structures"
date: 2017-10-14 12:56:43 -0500
tags: [swift, codable]
---

There is much pomp and circumstance over the new Swift `Codable` protocol. After indulging myself in the documentation and examples for a few hours, I couldn't help but feel there was a glaring omission: reusable structures.

In several projects that I've worked on I have found a need for a common contract between responses. For instance, I may want to know if the request had a successful operation–– and if not, perhaps a message informing me what the server had to say about it. This essentially creates a wrapper around the otherwise simple payload structure. To make the example a bit more interesting I'll convolute the common data with another wrapper around it. Thus, the payload will be contained within a result structure which is confined to a frivolous wrapper, like so:

    {
      "response": {
        "successful": true,
        "message": null,
        "payload": {
          "first_name": "Ryan",
          "last_name": "Maroon",
          "hobby": "Programming"
        }
      }
    }

Rather than constructing a very elaborate decoder path or rewriting the same boilerplate code for each structure, such behavior can be accommodated with the use of generics.

    struct Response<T: Codable>: Codable {
      let result: Result<T>

      private enum CodingKeys: String, CodingKey {
        case result = "response"
      }
    }

    struct Result<T: Codable>: Codable {
      let successful: Bool
      let message: String?
      let payload: T
    }

Swift will now parse any payload data so long as it conforms to the `Codable` protocol. We'll take a look at another example of this in a bit. First, let's take a look at the response payload representing the person data set.

    {
      "first_name": "Ryan",
      "last_name": "Maroon",
      "hobby": "Programming"
    }

The structure for such a response would then be described as such:

    struct Person: Codable {
      let firstName: String
      let lastName: String
      let hobby: String

      private enum CodingKeys: String, CodingKey {
        case firstName = "first_name"
        case lastName = "last_name"
        case hobby
      }
    }

Finally, the decoder would take the response data and the generic wrapper like this:

    let decoder = JSONDecoder()
    let payload = try! decoder.decode(Response<Person.self>, from: responseData)

That's it! Another example, as mentioned earlier, would be to utilize an array which also conforms to `Codable`. Assuming this is the payload from the server:

    [
      "Haskell",
      "Elixir",
      "Elm",
    ]

The decoder call would then be expressed as:

    let payload = try! decoder.decode(Response<[String]>.self, from: responseData)

