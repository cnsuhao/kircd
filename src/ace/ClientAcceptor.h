#ifndef KIRCD_CLIENT_ACCEPTOR_H
#define KIRCD_CLIENT_ACCEPTOR_H

#include <ace/INET_Addr.h>
#include <ace/SOCK_Acceptor.h>
#include <ace/Acceptor.h>
#include "ClientService.h"

typedef ACE_Acceptor<ClientService, ACE_SOCK_ACCEPTOR> ClientAcceptor;

#endif
