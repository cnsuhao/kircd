#include "ClientService.h"

int ClientService::open(void *p)
{
	if ( super::open(p) == -1 )
		return -1;

	ACE_TCHAR peer_name[MAXHOSTNAMELEN];
	ACE_INET_Addr peer_addr;

	if ( this->peer().get_remote_addr(peer_addr) == 0 &&
		peer_addr.addr_to_string(peer_name, MAXHOSTNAMELEN) == 0 )
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("(%P|%t) Connection from %s\n"), peer_name));
	}

	return 0;
}

int ClientService::handle_input(ACE_HANDLE fd)
{
	const size_t INPUT_SIZE = 4096;
	char buffer[INPUT_SIZE];
	ssize_t recv_cnt, send_cnt;

	if ( (recv_cnt = this->peer().recv(buffer, sizeof(buffer))) <= 0 )
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("(%P|%t) Connection closed\n")));
		return -1;
	}

	send_cnt = this->peer().send(buffer, ACE_static_cast(size_t, recv_cnt));
	if ( send_cnt == recv_cnt )
		return 0;
	if ( send_cnt == -1 && ACE_OS::last_error() != EWOULDBLOCK )
		ACE_ERROR_RETURN((LM_ERROR, ACE_TEXT("(%P|%t) %p\n"), ACE_TEXT("send")), 0);
	if ( send_cnt == -1 )
		send_cnt = 0;

	ACE_Message_Block *mb;
	size_t remaining = ACE_static_cast(size_t, (recv_cnt - send_cnt));
	ACE_NEW_RETURN(mb, ACE_Message_Block(&buffer[send_cnt], remaining), -1);
	int output_off = this->msg_queue()->is_empty();
	ACE_Time_Value nowait(ACE_OS::gettimeofday());
	if ( this->putq(mb, &nowait) == -1 )
	{
		ACE_ERROR((LM_ERROR, ACE_TEXT("(%P|%t) %p; discarding data\n"),
			ACE_TEXT("enqueue failed")));
		mb->release();
		return 0;
	}

	if (output_off)
		return this->reactor()->register_handler(this, ACE_Event_Handler::WRITE_MASK);

	return 0;
}

int ClientService::handle_output(ACE_HANDLE fd)
{
	ACE_Message_Block *mb;
	ACE_Time_Value nowait(ACE_OS::gettimeofday());
	while (0 == this->getq(mb, &nowait))
	{
		ssize_t send_cnt = this->peer().send(mb->rd_ptr(), mb->length());
		if (send_cnt == -1)
			ACE_ERROR((LM_ERROR, ACE_TEXT("(%P|%t) %p\n"), ACE_TEXT("send")));
		else
			mb->rd_ptr(ACE_static_cast(size_t, send_cnt));
		if (mb->length() > 0)
			this->msg_queue()->enqueue_head(mb);
		mb->release();
	}
	return (this->msg_queue()->is_empty()) ? -1 : 0;
}

int ClientService::handle_close(ACE_HANDLE h, ACE_Reactor_Mask mask)
{
	if (mask == ACE_Event_Handler::WRITE_MASK) return 0;
	return super::handle_close(h, mask);
}

