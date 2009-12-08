#include <ace/Reactor.h>
#include "ClientAcceptor.h"

int ACE_MAIN(int, ACE_TCHAR *[])
{
	ACE_INET_Addr port_to_listen(6667);
	ClientAcceptor acceptor;
	acceptor.reactor(ACE_Reactor::instance());
	if ( acceptor.open(port_to_listen) == -1 )
		return 1;

	ACE_Reactor::instance()->run_reactor_event_loop();
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("quit normally!\n")));
	return 0;
}

