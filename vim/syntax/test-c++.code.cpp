#include <stdlib>
#include <vecrtor>

class MyClass {         // 'class' --> Structure
	public:                 // 'public' --> Statement
		int size() const;
	private:
		volatile float x = 90.0f;
};

// Foo function
int foo(void) {
	try {  // 'try' --> Exception
		throw new MyException();   // 'new' --> Statement
	} catch MyException& e {}
}
