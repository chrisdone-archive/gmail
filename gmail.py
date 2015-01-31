# Copyright (c) 2015 Chris Done. All rights reserved.

# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

from sexpdata import loads, dumps
import httplib2
import sys
from apiclient.discovery import build
from oauth2client.client import flow_from_clientsecrets
from oauth2client.file import Storage
from oauth2client.tools import run
import json

# Gmail API boilerplate

CLIENT_SECRET_FILE = 'client_secret.json'
OAUTH_SCOPE = 'https://www.googleapis.com/auth/gmail.modify'
STORAGE = Storage('gmail.storage')
flow = flow_from_clientsecrets(CLIENT_SECRET_FILE, scope=OAUTH_SCOPE)
http = httplib2.Http()
credentials = STORAGE.get()
if credentials is None or credentials.invalid:
  credentials = run(flow, STORAGE, http=http)
http = credentials.authorize(http)
gmail_service = build('gmail', 'v1', http=http)

# Workaround Python's ineptitude

def print_unicode(string):
  print string.encode('utf8', 'replace')

# Command dispatching code

cmds = loads(sys.argv[1])

print "("
for cmd in cmds:
  args = cmd
  if args[0] == 'threads':
    if args[1] == 'list':
      labels = args[2]
      q = args[3]
      threads = gmail_service.users().threads().list(userId='me',labelIds=labels,q=q).execute()
      print_unicode(dumps(threads))
    if args[1] == 'get':
      id = args[2]
      format = args[3]
      thread = gmail_service.users().threads().get(userId='me',id=id,format=format).execute()
      print_unicode(dumps(thread))
    if args[1] == 'modify':
      body = json.loads('{"removeLabelIds":[],"addLabelIds":[]}')
      id = args[2];
      body["addLabelIds"] = args[3];
      body["removeLabelIds"] = args[4];
      labels = gmail_service.users().threads().modify(userId='me',id=id,body=body).execute()
      print_unicode(dumps(labels))
  if args[0] == 'messages':
    if args[1] == 'list':
      labels = args[2]
      q = args[3]
      messages = gmail_service.users().messages().list(userId='me',labelIds=labels,q=q).execute()
      print_unicode(dumps(messages))
    if args[1] == 'get':
      id = args[2]
      format = args[3]
      message = gmail_service.users().messages().get(userId='me',id=id,format=format).execute()
      print_unicode(dumps(message))
    if args[1] == 'modify':
      body = json.loads('{"removeLabelIds":[],"addLabelIds":[]}')
      id = args[2];
      body["addLabelIds"] = args[3];
      body["removeLabelIds"] = args[4];
      labels = gmail_service.users().messages().modify(userId='me',id=id,body=body).execute()
      print_unicode(dumps(labels))
  if args[0] == 'drafts':
    if args[1] == 'list':
      drafts = gmail_service.users().drafts().list(userId='me').execute()
      print_unicode(dumps(drafts))
  if args[0] == 'profile':
    if args[1] == 'get':
      profile = gmail_service.users().getProfile(userId='me').execute()
      print_unicode(dumps(profile))
  if args[0] == 'labels':
    if args[1] == 'list':
      labels = gmail_service.users().labels().list(userId='me').execute()
      print_unicode(dumps(labels))
print ")"
