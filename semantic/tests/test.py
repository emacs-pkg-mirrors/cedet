# Test file for Python language.
#
# $Id: test.py,v 1.1 2002/06/19 04:46:44 emacsman Exp $

# Simle class compount statement with blank lines sprinkled.
class Foo(Bar):

    x = 1

    y = 2

# Simple def statement with no argument
def sss():
    i = 1

# Simple def statement with arguments
def ttt(x,y,z):
    i = 1

import foo

for x in y:
    print x

while y > 0:
    y = y - 1

a=b=c=d=e=f=i=j=k=l=m=n=o=p=q=r=s=t=x=y=1

if x:
    x = 2
    y = 3

x = 2
y = 3
s and t
q | r
o ^ p
m & n
k << l
z = 4
i >> j
e / f
c * d
a + b
2 ** 5
x
s = "a" "b" "c"
1

# wisent-python.wy chokes on this! -ryk 6/17/02

#class HTTPServer(xxx.yyy):
#    allow_reuse_address = 1    # Seems to make sense in testing environment
#    def server_bind(self):
#        SocketServer.TCPServer.server_bind(self)
#        host, port = self.socket.getsockname()
#        self.server_name = socket.getfqdn(host)
#        self.server_port = port


