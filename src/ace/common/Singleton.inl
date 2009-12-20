
template <typename T>
T& Singleton<T>::Instance()
{
	if ( instance_ == NULL )
	{
		instance_ = new T;
	}

	return *instance_;
}

