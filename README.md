# websocket-channel-server

A generic standalone websocket server for all of your signalling needs.

## How it works


A websocket-channel-server server manages a set of independent channels.
Clients who wish to communicate with each other should all connect to the same
channel. The connection is done through websockets, with the channel name given
by the URL path.  When you connect, you'll be greeted:

    {"members":[1],"yourid":1,"type":"welcome"}

Then, you can send messages. They can be whatever your application needs.
They'll be relayed to all the other members of the channel:

    {"from":1,"type":"peermessage","message":"hello"}

When someone enters or leaves the channel, you'll be notified:

    {"id":2,"type":"join"}
    {"id":2,"type":"disconnect"}
    
