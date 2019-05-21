import math as _m

def _procArg(x):
    if 0 <= x < 2 * _m.pi:
        return x
    elif x >= _m.pi:
        return _procArg(x - 2 * _m.pi)
    else:
        return _procArg(x + 2 * _m.pi)

class cplx():
    '''re: real part; im: imaginary part'''
    def __init__(self, re=0, im=0):
        self.re = re
        self.im = im
    # returns a printable string to represent the class
    def __repr__(self):
        if self.re != 0 and self.im > 0:
            return '(' + str(self.re) + '+' + str(self.im) + 'j)'
        elif self.re != 0 and self.im < 0:
            return '(' + str(self.re) + str(self.im) + 'j)'
        elif self.re != 0 and self.im == 0:
            return str(self.re)
        elif self.re == 0 and self.im != 0:
            return str(self.im) + 'j'
        else:
            return '0'
    def isreal(self):
        return self.im == 0
    def isimaginary(self):
        return self.re == 0
    def iszero(self):
        return self.im == 0 and self.re == 0
    def real(self):
        return self.re if self.isreal() else self
    def mod(self):
        return pow(self.re * self.re + self.im * self.im, 1/2)
    def arg(self, *unit):
        '''
        unit='r' (default) means displaying result in radians.
        unit='d' displays result in degrees.'''
        res = _m.atan2(self.im, self.re)
        if len(unit) > 0 and unit[0] == 'd':
            return 180 * res / _m.pi
        else:
            return res
    def neg(self):
        return cplx(-self.re, -self.im)
    def cgrc(self):
        return cplx(self.re, -self.im)
    ## two nums: a+bj and c+dj
    def __typecheck(self, other):
        a, b = self.re, self.im
        if isinstance(other, cplx):
            c, d = other.re, other.im
        elif isinstance(other, rcplx):
            c, d = other.real(), other.imag()
        else:
            c, d = other, 0
        return (a, b, c, d)
    # overriding == operator
    def __eq__(self, other, prec=0.000001):
        'Default precision applies. (0.000001)'
        a, b, c, d = self.__typecheck(other)
        aeq = lambda x, y: abs(x - y) < prec
        return aeq(a, c) and aeq(b, d)
    # overriding != operator
    def __ne__(self, other):
        return not self.__eq__(other)
    # overriding + operator
    def __add__(self, other):
        a, b, c, d = self.__typecheck(other)
        return cplx(a + c, b + d)
    # overriding - operator
    def __sub__(self, other):
        a, b, c, d = self.__typecheck(other)
        return cplx(a - c, b - d)
    # overriding * operator
    def __mul__(self, other):
        a, b, c, d = self.__typecheck(other)
        return cplx(a * c - b * d, b * c + a * d)
    # overriding / operator
    def __truediv__(self, other):
        a, b, c, d = self.__typecheck(other)
        denum = c * c + d * d
        return cplx((a * c + b * d) / denum, (b * c - a * d) / denum)
    # overriding ** operator
    def __pow__(self, power):
        arg = self.arg()
        return cplx(_m.cos(power * arg), _m.sin(power * arg)) * pow(self.mod(), power)
#j = cplx(0, 1)

class rcplx:
    'r is modulus; a is argument in radians.'
    def __init__(self, r=0, a=0):
        if r < 0:
            raise ValueError('Modulo must be nonnegative.')
        self.r = r
        self.a = _procArg(a)
    def __repr__(self):
        return str(self.r) + '∠' + str(self.a)
    def real(self):
        return self.r * _m.cos(self.a)
    def imag(self):
        return self.r * _m.sin(self.a)
    def neg(self):
        return rcplx(self.r, self.a + _m.pi)
    def cgrc(self):
        return rcplx(self.r, -self.a)
    def __typecheck(self, other):
        r1, a1 = self.r, self.a
        if isinstance(other, rcplx):
            r2, a2 = other.r, other.a
        elif isinstance(other, cplx):
            r2, a2 = other.mod(), other.arg()
        else:
            if other > 0:
                r2, a2 = other, 0
            elif other < 0:
                r2, a2 = -other, _m.pi
            else:
                r2, a2 = 0, 0
        return (r1, a1, r2, a2)
    def __eq__(self, other, rprec=0.0001, aprec=0.0001):
        'Default precision applies. (0.0001) rprec is for modulus, aprec is for argument.'
        r1, a1, r2, a2 = self.__typecheck(other)
        aeq = lambda x, y, e: abs(x - y) < e
        return aeq(r1, r2, rprec) and aeq(a1, a2, aprec)
    def __ne__(self, other, prec=0.0001):
        'Default precision applies. (0.0001) rprec is for modulus, aprec is for argument.'
        return not self.__eq__(other, prec)
    def __add__(self, other):
        r1, a1, r2, a2 = self.__typecheck(other)
        real = r1 * _m.cos(a1) + r2 * _m.cos(a2)
        imag = r1 * _m.sin(a1) + r2 * _m.sin(a2)
        return rcplx(pow(real * real + imag * imag, 1/2), _m.atan2(imag, real))
    def __sub__(self, other):
        r1, a1, r2, a2 = self.__typecheck(other)
        real = r1 * _m.cos(a1) - r2 * _m.cos(a2)
        imag = r1 * _m.sin(a1) - r2 * _m.sin(a2)
        return rcplx(pow(real * real + imag * imag, 1/2), _m.atan2(imag, real))
    def __mul__(self, other):
        r1, a1, r2, a2 = self.__typecheck(other)
        return rcplx(r1 * r2, a1 + a2)
    def __truediv__(self, other):
        r1, a1, r2, a2 = self.__typecheck(other)
        return rcplx(r1 / r2, a1 - a2)
    def __pow__(self, power):
        return rcplx(pow(self.r, power), power * self.a)

def cplx2rcplx(mycplx):
    return rcplx(mycplx.mod(), mycplx.arg())
def rcplx2cplx(myrcplx):
    return cplx(myrcplx.real(), myrcplx.imag())
