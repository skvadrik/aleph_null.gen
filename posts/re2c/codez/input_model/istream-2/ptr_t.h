#include <sstream>

class ptr_t
{
    std::istringstream & is_;

  public:
    explicit ptr_t (std::istringstream & is)
        : is_ (is)
    {}

    ptr_t & operator ++ ()
    {
        is_.ignore ();
        return * this;
    }

    int operator * ()
    {
        return is_.peek ();
    }

    operator std::streampos ()
    {
        return is_.tellg ();
    }

    ptr_t & operator = (std::streampos p)
    {
        is_.seekg (p);
        return *this;
    }
};
