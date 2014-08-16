#!/usr/bin/env python

from twisted.internet import reactor, protocol

reply = "toto"

class Echo(protocol.Protocol):
    """This is just about the simplest possible protocol"""

    def dataReceived(self, data):
        "As soon as any data is received, write it back."
        print "rcvd: %s" % repr(data)
        print "sent: %s" % repr(reply)
        self.transport.write(reply)


def main():
    """This runs the protocol on port 8000"""
    factory = protocol.ServerFactory()
    factory.protocol = Echo
    reactor.listenTCP(9090, factory)
    reactor.run()

# this only runs if the module was *not* imported
if __name__ == '__main__':
    main()
