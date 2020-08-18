# Generate UDP messages for testing.
import argparse
import os, sys
import socket
import datetime
import time
import random
import math
import codecs

usock = None

# Send a short text over UDP.
def send_remote( txt ):
  global usock
  if usock:
    msg = 'T' + txt
    data = codecs.encode(msg)
    n = usock.sendto(data, ('255.255.255.255', 5399) )

# Look for broadcast UDP messages telling us to take pictures.  We will
# take a series of pictures very quickly and save them at full resolution.
def check_request( force ):
  global now, usock, confidence, category

  # Look for an incoming UDP message
  try:
    request, address = usock.recvfrom(100)
  except socket.error:
    request = None

  if request:
    # Got something.  Convert to string.
    reqmsg = request.decode('utf-8')

    # We only care about SNAP messages
    if reqmsg == 'S':

      logevent("Request")

      # Take a series of pictures as quickly as possible (about every .4 sec),
      # without scaling to Tensorflow's size.  We want to preserve maximum
      # resolution.  Filename has fractional seconds.
      need_check = True
      for n in range(1,21):
        if acquire( False ):  # Grab unscaled image
          now = datetime.datetime.now()
          filedate = now.strftime( '%m%d-%H%M-%S.%f' )
          imgfile = '%s/request/%d-%s.jpg' % (FLAGS.dir, camnum, filedate)
          if need_check:
            # Only need to check this once
            ensure_dir( imgfile )
            need_check = False
          # Save the image
          sample.save( imgfile )
      # All done
      send_remote( 'Done' )

  # Check for more UDP messages twice per second
  window.after( 550, check_request, None )

####################
# Initialization starts here
####################

# Get options from command line
p = argparse.ArgumentParser()
p.add_argument('--msg', default='MAIL', help='Short text')

FLAGS, unparsed = p.parse_known_args()

try:
  usock = socket.socket( socket.AF_INET, socket.SOCK_DGRAM )
  usock.setblocking(False)
  usock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
  usock.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)
except socket.error as msg:
  print("Socket error %s\n" % msg[1] )

# 300, 'The mail has arrived', 'MAIL', 'blue', 'white'
# 60, 'The trash has been collected', 'GARB', 'brown', 'white'
#  1, 'A car drove by', 'CAR', 'dark green', 'white'
# 60, 'We have a visitor', 'DWAY', 'orange', 'black'
# =  1, 'People walking', 'WALK', 'yellow2', 'black'
# = 99, 'A package was delivered', 'PKG',  'brown4', 'gold'

send_remote( FLAGS.msg )
