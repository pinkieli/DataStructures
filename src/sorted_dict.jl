"""
    type SortedDict{K,V,Ord} <: Associative{K,V}

A SortedDict is a wrapper around balancedTree with
methods similiar to those of Julia container Dict.
"""
type SortedDict{K,V,Ord<:Ordering} <: Associative{K,V}
    bt::BalancedTree23{K,V,Ord}

    ## Base constructors
    SortedDict(o::Ord) = new(BalancedTree23{K,V,Ord}(o))

    function SortedDict(o::Ord, kv)
        s = new(BalancedTree23{K,V,Ord}(o))

        if eltype(kv) <: Pair
            # It's (possibly?) more efficient to access the first and second
            # elements of Pairs directly, rather than destructure
            for p in kv
                s[p.first] = p.second
            end
        else
            for (k,v) in kv
                s[k] = v
            end
        end
        return s
    end

end

# Any-Any constructors
"""
    SortedDict(o::Ord=Forward)
    SortedDict{K,V}(o::Ord=Forward)
    SortedDict(o::Ord, kv)
    SortedDict{K,V}(o::Ord, kv)
    SortedDict(k1=>v1, k2=>v2, ...)
    SortedDict{K,V}(k1=>v1, k2=>v2, ...)
    SortedDict(o::Ord, k1=>v1, k2=>v2, ...)
    SortedDict{K,V}(o::Ord, k1=>v1, k2=>v2, ...)

Create a new ``SortedDict`` from an ``Ordering`` and/or an interable of
key-values or key-value pairs.  Key type ``K`` and value type ``V`` may be
specified in the constructor.  If they are not specified, they are inferred
from the given iterable or key-value pairs.  If they are not specified and
no additional inputs are available, ``K`` and ``V`` default to ``Any``.

**Note that a key type of ``Any`` will lead to slow performance, as the
values are stored boxed (i.e., as pointers), and insertion will
require a run-time lookup of the appropriate comparison function.
It is recommended to always specify a key type, or to use one of the
constructors below in which the key type is inferred.**
"""
SortedDict() = SortedDict{Any,Any,ForwardOrdering}(Forward)
SortedDict{Ord<:Ordering}(o::Ord) = SortedDict{Any,Any,Ord}(o)

function not_iterator_of_pairs(kv)
    return any(x->isempty(methodswith(typeof(kv), x, true)),
               [start, next, done]) ||
           any(x->!isa(x, Union{Tuple,Pair}), kv)
end

# Construction from Pairs
# TODO: fix SortedDict(1=>1, 2=>2.0)
SortedDict(ps::Pair...) = SortedDict(Forward, ps)
SortedDict(o::Ordering, ps::Pair...) = SortedDict(o, ps)
@compat (::Type{SortedDict{K,V}}){K,V}(ps::Pair...) = SortedDict{K,V,ForwardOrdering}(Forward, ps)
@compat (::Type{SortedDict{K,V}}){K,V,Ord<:Ordering}(o::Ord, ps::Pair...) = SortedDict{K,V,Ord}(o, ps)

# Construction from Associatives
SortedDict{K,V,Ord<:Ordering}(o::Ord, d::Associative{K,V}) = SortedDict{K,V,Ord}(o, d)

## Construction from iteratables of Pairs/Tuples

# Construction specifying Key/Value types
# e.g., SortedDict{Int,Float64}([1=>1, 2=>2.0])
@compat (::Type{SortedDict{K,V}}){K,V}(kv) = SortedDict{K,V}(Forward, kv)
@compat function (::Type{SortedDict{K,V}}){K,V,Ord<:Ordering}(o::Ord, kv)
    try
        SortedDict{K,V,Ord}(o, kv)
    catch e
        if not_iterator_of_pairs(kv)
            throw(ArgumentError("SortedDict(kv): kv needs to be an iterator of tuples or pairs"))
        else
            rethrow(e)
        end
    end
end

# Construction inferring Key/Value types from input
# e.g. SortedDict{}
SortedDict(kv, o::Ordering=Forward) = SortedDict(o, kv)
function SortedDict(o::Ordering, kv)
    try
        _sorted_dict_with_eltype(o, kv, eltype(kv))
    catch e
        if not_iterator_of_pairs(kv)
            throw(ArgumentError("SortedDict(kv): kv needs to be an iterator of tuples or pairs"))
        else
            rethrow(e)
        end
    end
end

_sorted_dict_with_eltype{K,V,Ord}(o::Ord, ps, ::Type{Pair{K,V}} ) = SortedDict{  K,  V,Ord}(o, ps)
_sorted_dict_with_eltype{K,V,Ord}(o::Ord, kv, ::Type{Tuple{K,V}}) = SortedDict{  K,  V,Ord}(o, kv)
_sorted_dict_with_eltype{K,  Ord}(o::Ord, ps, ::Type{Pair{K}}   ) = SortedDict{  K,Any,Ord}(o, ps)
_sorted_dict_with_eltype{    Ord}(o::Ord, kv, ::Type            ) = SortedDict{Any,Any,Ord}(o, kv)

## TODO: It seems impossible (or at least very challenging) to create the eltype below.
##       If deemed possible, please create a test and uncomment this definition.
# if VERSION < v"0.6.0-dev.2123"
#     _sorted_dict_with_eltype{  V,Ord}(o::Ord, ps, ::Type{Pair{TypeVar(:K),V}}) = SortedDict{Any,  V,Ord}(o, ps)
# else
#     eval(parse("_sorted_dict_with_eltype{  V,Ord}(o::Ord, ps, ::Type{Pair{K,V} where K}) = SortedDict{Any,  V,Ord}(o, ps)"))
# end


typealias SDSemiToken IntSemiToken

typealias SDToken Tuple{SortedDict,IntSemiToken}

## This function implements m[k]; it returns the
## data item associated with key k.

@inline function getindex(m::SortedDict, k_)
    i, exactfound = findkey(m.bt, convert(keytype(m),k_))
    !exactfound && throw(KeyError(k_))
    return m.bt.data[i].d
end


## This function implements m[k]=d; it sets the
## data item associated with key k equal to d.

@inline function setindex!{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord}, v_, k_)
    insert!(m.bt, convert(K,k_), convert(V,v_), false)
    m
end

## push! is an alternative to insert!; it returns the container.


@inline function push!{K,V}(m::SortedDict{K,V}, pr::Pair)
    insert!(m.bt, convert(K, pr[1]), convert(V, pr[2]), false)
    m
end




## This function looks up a key in the tree;
## if not found, then it returns a marker for the
## end of the tree.


@inline function find(m::SortedDict, k_)
    ll, exactfound = findkey(m.bt, convert(keytype(m),k_))
    IntSemiToken(exactfound? ll : 2)
end

## This function inserts an item into the tree.
## Unlike m[k]=d, it also returns a bool and a token.
## The bool is true if the inserted item is new.
## It is false if there was already an item
## with that key.
## The token points to the newly inserted item.


@inline function insert!{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord}, k_, v_)
    b, i = insert!(m.bt, convert(K,k_), convert(V,v_), false)
    b, IntSemiToken(i)
end



@inline eltype{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord}) =  Pair{K,V}
@inline eltype{K,V,Ord<:Ordering}(::Type{SortedDict{K,V,Ord}}) =  Pair{K,V}
@inline function in{K,V,Ord<:Ordering}(pr::Pair, m::SortedDict{K,V,Ord})
    i, exactfound = findkey(m.bt,convert(K,pr[1]))
    return exactfound && isequal(m.bt.data[i].d,convert(V,pr[2]))
end

@inline in(::Tuple{Any,Any}, ::SortedDict) =
    throw(ArgumentError("'(k,v) in sorteddict' not supported in Julia 0.4 or 0.5.  See documentation"))


@inline keytype{K,V,Ord<:Ordering}(::Type{SortedDict{K,V,Ord}}) = K
@inline valtype{K,V,Ord<:Ordering}(::Type{SortedDict{K,V,Ord}}) = V
@inline ordtype{K,V,Ord<:Ordering}(::Type{SortedDict{K,V,Ord}}) = Ord


## First and last return the first and last (key,data) pairs
## in the SortedDict.  It is an error to invoke them on an
## empty SortedDict.


@inline function first(m::SortedDict)
    i = beginloc(m.bt)
    i == 2 && throw(BoundsError())
    return Pair(m.bt.data[i].k, m.bt.data[i].d)
end

@inline function last(m::SortedDict)
    i = endloc(m.bt)
    i == 1 && throw(BoundsError())
    return Pair(m.bt.data[i].k, m.bt.data[i].d)
end




@inline orderobject(m::SortedDict) = m.bt.ord


@inline function haskey(m::SortedDict, k_)
    i, exactfound = findkey(m.bt,convert(keytype(m),k_))
    exactfound
end

@inline function get{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord}, k_, default_)
    i, exactfound = findkey(m.bt, convert(K,k_))
   return exactfound? m.bt.data[i].d : convert(V,default_)
end


function get!{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord}, k_, default_)
    k = convert(K,k_)
    i, exactfound = findkey(m.bt, k)
    if exactfound
        return m.bt.data[i].d
    else
        default = convert(V,default_)
        insert!(m.bt,k, default, false)
        return default
    end
end


function getkey{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord}, k_, default_)
    i, exactfound = findkey(m.bt, convert(K,k_))
    exactfound? m.bt.data[i].k : convert(K, default_)
end

## Function delete! deletes an item at a given
## key

@inline function delete!(m::SortedDict, k_)
    i, exactfound = findkey(m.bt,convert(keytype(m),k_))
    !exactfound && throw(KeyError(k_))
    delete!(m.bt, i)
    m
end

@inline function pop!(m::SortedDict, k_)
    i, exactfound = findkey(m.bt,convert(keytype(m),k_))
    !exactfound && throw(KeyError(k_))
    d = m.bt.data[i].d
    delete!(m.bt, i)
    d
end


## Check if two SortedDicts are equal in the sense of containing
## the same (K,V) pairs.  This sense of equality does not mean
## that semitokens valid for one are also valid for the other.

function isequal(m1::SortedDict, m2::SortedDict)
    ord = orderobject(m1)
    if !isequal(ord, orderobject(m2)) || !isequal(eltype(m1), eltype(m2))
        throw(ArgumentError("Cannot use isequal for two SortedDicts unless their element types and ordering objects are equal"))
    end
    p1 = startof(m1)
    p2 = startof(m2)
    while true
        if p1 == pastendsemitoken(m1)
            return p2 == pastendsemitoken(m2)
        end
        if p2 == pastendsemitoken(m2)
            return false
        end
        k1,d1 = deref((m1,p1))
        k2,d2 = deref((m2,p2))
        if !eq(ord,k1,k2) || !isequal(d1,d2)
            return false
        end
        p1 = advance((m1,p1))
        p2 = advance((m2,p2))
    end
end


function mergetwo!{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord},
                                        m2::Associative{K,V})
    for (k,v) in m2
        m[convert(K,k)] = convert(V,v)
    end
end

function packcopy{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord})
    w = SortedDict(Dict{K,V}(),orderobject(m))
    mergetwo!(w,m)
    w
end

function packdeepcopy{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord})
    w = SortedDict(Dict{K,V}(),orderobject(m))
    for (k,v) in m
        newk = deepcopy(k)
        newv = deepcopy(v)
        w[newk] = newv
    end
    w
end


function merge!{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord},
                                     others::Associative{K,V}...)
    for o in others
        mergetwo!(m,o)
    end
end

function merge{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord},
                                    others::Associative{K,V}...)
    result = packcopy(m)
    merge!(result, others...)
    result
end



similar{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord}) =
    SortedDict{K,V,Ord}(orderobject(m))
