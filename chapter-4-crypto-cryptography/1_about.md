# About Cryptography

Cryptography is the scientific art of concealing data in a manner that makes it impractical or highly time-consuming to reverse engineer the encoded message. In the context of our scenario, where we aim to transmit a confidential message from one party to another, we'll adopt the commonly used actor names Alice and Bob.

Good old Alice and Bob! We'll get used to hearing about their adventures throughout the book.

Let's say Alice and Bob are two parties who have never met, but want to communicate securely over the internet. How could they do this? In this thought experiment, we'll assume that any communication sent by Alice will be intercepted and read by unauthorized parties.

Perhaps the first idea we might come up with is to share some password to encrypt a message, but if the communications are always read in transit, how do we send the password? Perhaps the parties could arrange to meet physically and share some code or password and assume that they're not being watched, but this of course is pretty impractical.

In 1977 Ron Rivest, Adi Shamir, and Leonard Adleman from MIT invented the RSA algorithm that is widely used for secure data transmission.

In this chapter, we will develop a deeper understanding of the RSA algorithm, and other fundamental cryptographic algorithms that are essential to our application.