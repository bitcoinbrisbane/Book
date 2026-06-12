## Communications layer

For our application to work, it must communicate to other clients or nodes on the network. For a blockchain based application, the architecture is different from a traditional web server approach. Let's explore a few options with a toy "chat" application where users can chat over the internet like WhatsApp, Signal, Telegram or many other chat applications.

## Client/server and REST

Web browsing is probably the most common use of the internet today. This involves a client server architecture which implements a few protocols, such as TCP, HTTP and REST. Not all websites must use the REST protocol, but for now let's assume they do. So in this use case, we can define a client as a user's browser. Mobile or desktop, Chrome or Safari it doesn't matter as vendors of the software, Google and Apple should develop their browser to the agreed standards so that all websites are viewed the same regardless of the browser.

When a user enters a URL in their browser such as www.google.com, the browser makes an HTTP GET request to a server at Google. The server that answers the request would run a webserver application like Apache, that handles the request and based on the website application, knows how to handle the request and return a response.

Our webserver will define REST routes that know how to handle the inbound request.

In a chat scenario, the top diagram represents the client from Alice's perspective, and the bottom from Bob's perspective. In this type of application, it's typical to have a server that takes inbound messages and stores them in some kind of database.

By having a server, the IP or domain is known and can be easily shared to potential users. The owner of the server can create firewall rules to allow the inbound connections from clients on the network — normally to everyone but only to specific ports. In the case of HTTP web protocol, that is normally 80 and 443. We will see why this can be a challenge in other peer to peer architectures, but for now we will continue with this approach.

## The polling problem

The problem with the above architecture if it is based on REST is that if the state of the server changes, the client is unaware. With HTTP request/response, the only way for the client to find out is to poll the server. It can wait a certain amount of time and then query the server again and again. There is a concept of long polling, but it's pretty much the same.

## WebSockets

WebSockets are a protocol that provides full-duplex communication channels over a single TCP connection. They are designed to be implemented in web browsers and web servers, but can be used by any client or server application. WebSockets enable interactive communication between a user's browser and a server, allowing real-time updates and bidirectional data flow.

For our application, we will subscribe to:

- Node to node to listen for new blocks
- Changes to game state
- And our application to listen to the game state changes

In the previous chapter, Node A discovers Node B through the protocol...
