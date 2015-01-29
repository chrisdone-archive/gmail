#!/usr/bin/python

from sexpdata import loads, dumps

import httplib2
import sys

from apiclient.discovery import build
from oauth2client.client import flow_from_clientsecrets
from oauth2client.file import Storage
from oauth2client.tools import run


# Path to the client_secret.json file downloaded from the Developer Console
CLIENT_SECRET_FILE = 'client_secret.json'

# Check https://developers.google.com/gmail/api/auth/scopes for all available scopes
OAUTH_SCOPE = 'https://www.googleapis.com/auth/gmail.modify'

# Location of the credentials storage file
STORAGE = Storage('gmail.storage')

# Start the OAuth flow to retrieve credentials
flow = flow_from_clientsecrets(CLIENT_SECRET_FILE, scope=OAUTH_SCOPE)
http = httplib2.Http()

# Try to retrieve credentials from storage or run the flow to generate them
credentials = STORAGE.get()
if credentials is None or credentials.invalid:
  credentials = run(flow, STORAGE, http=http)

# Authorize the httplib2.Http object with our credentials
http = credentials.authorize(http)

# Build the Gmail service from discovery
gmail_service = build('gmail', 'v1', http=http)

if sys.argv[1] == 'drafts':
  if sys.argv[2] == 'list':
    # Retrieve a page of threads
    drafts = gmail_service.users().drafts().list(userId='me').execute()
    print dumps(drafts);
if sys.argv[1] == 'labels':
  if sys.argv[2] == 'list':
    # Retrieve a page of threads
    labels = gmail_service.users().labels().list(userId='me').execute()
    print dumps(labels);
if sys.argv[1] == 'profile':
  # Retrieve a page of threads
  profile = gmail_service.users().getProfile(userId='me').execute()
  print dumps(profile);
if sys.argv[1] == 'threads':
  if sys.argv[2] == 'list':
    # Retrieve a page of threads
    threads = gmail_service.users().threads().list(userId='me',labelIds=["INBOX","UNREAD"]).execute()
    print dumps(threads);
  if sys.argv[2] == 'get':
    # Retrieve a page of threads
    thread = gmail_service.users().threads().get(userId='me',id=sys.argv[3],format='metadata').execute()
    print dumps(thread);
if sys.argv[1] == 'messages':
  if sys.argv[2] == 'list':
    # Retrieve a page of messages
    messages = gmail_service.users().messages().list(userId='me',labelIds="INBOX").execute()
    print dumps(messages);
  if sys.argv[2] == 'get':
    # Retrieve a page of messages
    message = gmail_service.users().messages().get(userId='me',id=sys.argv[3],format='full').execute()
    print dumps(message);
