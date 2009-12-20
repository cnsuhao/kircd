#ifndef KIRCD_SINGLETON_H
#define KIRCD_SINGLETON_H

// Usage:
//
// /* MyFoo.h */
// class MyFoo { ... };
// #define aMyFoo kIRCd::Singleton<MyFoo>::Instance()
//
// /* MyFoo.cpp */
// INSTANTIATE_SINGLETON(MyFoo);
//
// /* Other File */
// #include <MyFoo.h>
// aMyFoo.foobar();

namespace kIRCd
{

template <typename T>
class Singleton
{
public:
	static T& Instance();

protected:
	Singleton() {}

private:
	Singleton(const Singleton&);
	Singleton& operator=(const Singleton&);

private:
	static T* instance_;
};

#define INSTANTIATE_SINGLETON(T) \
	template class kIRCd::Singleton<T>; \
	template<> T* kIRCd::Singleton<T>::instance_ = 0

#include "Singleton.inl"

};

#endif
