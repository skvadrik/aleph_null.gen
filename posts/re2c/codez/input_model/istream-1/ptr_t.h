#include <sstream>
class ptr_t
{
    std::istringstream * is_;
    std::streampos p_;

  public:
    ptr_t (std::istringstream * is, std::streampos p)
        : is_ (is)
        , p_ (p)
    {}

    int operator * ()
    {
        is_->seekg (p_);
        return is_->peek ();
    }

    ptr_t & operator ++ ()
    {
        p_ += 1;
        return * this;
    }

    ptr_t operator + (std::streamoff off)
    {
        return ptr_t (is_, p_ + off);
    }

    friend bool operator <= (const ptr_t & ptr1, const ptr_t & ptr2)
    {
        return ptr1.p_ <= ptr2.p_;
    }

    friend std::streamoff operator - (const ptr_t & ptr1, const ptr_t & ptr2)
    {
        return ptr1.p_ - ptr2.p_;
    }
};
