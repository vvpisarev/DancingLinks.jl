module DancingLinks

export Link, LinkColumn, LinkMatrix, RestRow, FullRow
export insert_row!, find_col, id, algorithm_x!, complete_algorithm_x!, cover!

mutable struct Link{T}
    left::Link{T}
    right::Link{T}
    above::Link{T}
    below::Link{T}
    col::T
    function Link(col::T) where T
        result = new{T}()
        result.above = result.below = result.right = result.left = result
        result.col = col
        return result
    end
    function Link{T}() where T
        result = new{T}()
        result.above = result.below = result.right = result.left = result
        return result
    end
end

mutable struct LinkColumn{T}
    links::Link{LinkColumn{T}}
    size::Int
    id::T
    function LinkColumn{T}(id) where T
        result = new{T}()
        links = Link(result)
        result.links = links
        result.size = 0
        result.id = id
        return result
    end
end

LinkColumn(id::T) where T = LinkColumn{T}(id)

struct LinkMatrix{H<:LinkColumn}
    columns::Link{H}
    function LinkMatrix{H}(names) where {H<:LinkColumn}
        cols = Link{H}()
        root = new{H}(cols)
        prev = cols
        for name in names
            col = H(name)
            collinks = links(col)
            collinks.left = prev
            collinks.right = cols
            prev.right = collinks
            cols.left = collinks
            prev = collinks
        end
        return root
    end
end

columns(dlmatr::LinkMatrix) = dlmatr.columns

function LinkMatrix(names)
    T = LinkColumn{eltype(names)}
    return LinkMatrix{T}(names)
end

Base.show(io::IO, col::LinkColumn) = print(io, "Column $(id(col)) of size $(col.size)")

Base.show(io::IO, dl::Link) = print(io, "Link in column $(id(dl)) of length $(dl.col.size)")

Base.show(io::IO, dlmatr::LinkMatrix) = print(io, "Dancing links matrix")

id(col::LinkColumn) = col.id
id(dl::Link) = id(dl.col)

Base.length(col::LinkColumn) = col.size

right(dl::Link) = dl.right
left(dl::Link) = dl.left
above(dl::Link) = dl.above
below(dl::Link) = dl.below

links(col::LinkColumn) = col.links

right(col::LinkColumn) = right(links(col))
left(col::LinkColumn) = left(links(col))
above(col::LinkColumn) = above(links(col))
below(col::LinkColumn) = below(links(col))

function Base.iterate(dlmatr::LinkMatrix, next = right(columns(dlmatr)))
    next === columns(dlmatr) && return
    return (next.col, right(next))
end

function Base.iterate(col::LinkColumn, next = below(links(col)))
    next === links(col) && return
    return (next, below(next))
end

struct RestRow{T<:Link}
    node::T
end

struct FullRow{T<:Link}
    node::T
end

function Base.iterate(row::RestRow, next = right(row.node))
    next === row.node && return
    return (next, right(next))
end

function Base.iterate(row::FullRow)
    node = row.node
    return (node, right(node))
end

function Base.iterate(row::FullRow{T}, next::T) where {T<:Link}
    next === row.node && return
    return (next, right(next))
end

function Base.iterate(dlmatr::Iterators.Reverse{<:LinkMatrix}, next = left(columns(dlmatr.itr)))
    next === columns(dlmatr.itr) && return
    return (next.col, left(next))
end

function Base.iterate(col::Iterators.Reverse{<:LinkColumn}, next = above(links(col.itr)))
    next === links(col.itr) && return
    return (next, above(next))
end

function Base.iterate(row::Iterators.Reverse{<:RestRow}, next = left(row.itr.node))
    next === row.itr.node && return
    return (next, left(next))
end

function Base.iterate(row::Iterators.Reverse{<:FullRow})
    node = row.itr.node
    return (left(node), left(node))
end

function Base.iterate(row::Iterators.Reverse{FullRow{T}}, next::T) where {T<:Link}
    next === row.itr.node && return
    return (left(next), left(next))
end

function Base.isempty(dlmatr::LinkMatrix)
    cols = columns(dlmatr)
    return cols === right(cols)
end

# add isempty function for LinkColumn
Function Base.isempty(linkcolumn::LinkColumn)
    return length(linkcolumn) == 0
end

function algorithm_x!(root::LinkMatrix{T}, solution=FullRow{Link{T}}[]) where {T}
    if isempty(root)
        return solution
    end
    col = choose_col(root)
    cover!(col)
    for node in col
        push!(solution, FullRow(node))
        for j in RestRow(node)
            cover!(j.col)
        end
        if !isnothing(algorithm_x!(root, solution))
            return solution
        end
        node = pop!(solution).node
        for j in Iterators.reverse(RestRow(node))
            uncover!(j.col)
        end
    end
    uncover!(col)
    return
end

# add function to search for all solution of dancing links

function all_algorithm_x!(root::LinkMatrix{T}, all_solution::Array{Any, 1}, solution = FullRow{Link{T}}[]) where {T}
    
    if isempty(root)
        push!(all_solution, copy(solution))
        return nothing
    end
    
    col = choose_col(root)
    
    if isempty(col)
        return nothing
    end
    
    cover!(col)
    
    for node in col
        push!(solution, FullRow(node))
        for j in RestRow(node)
            cover!(j.col)
        end
        
        all_algorithm_x!(root, all_solution, solution)

        node = pop!(solution).node
        for j in Iterators.reverse(RestRow(node))
            uncover!(j.col)
        end
    end
    
    uncover!(col)
    
    return nothing
end

function complete_algorithm_x!(root::LinkMatrix{T}) where T
    
    result = []
    all_algorithm_x!(root, result)
    return result
    
end

function choose_col(root)
    bestcol = right(columns(root)).col
    s = length(bestcol)
    for col in root
        l = length(col)
        if l < s
            s, bestcol = l, col
        end
    end
    return bestcol
end

function detach_rl!(dl)
    dl.right.left = dl.left
    dl.left.right = dl.right
    return dl
end

function detach_ab!(dl::Link)
    dl.above.below = dl.below
    dl.below.above = dl.above
    dl.col.size -= 1
    return dl
end

function cover!(col)
    detach_rl!(links(col))
    for node in col
        for elt in RestRow(node)
            detach_ab!(elt)
        end
    end
end

function restore_rl!(dl)
    dl.right.left = dl
    dl.left.right = dl
    return dl
end

function restore_ab!(dl::Link)
    dl.above.below = dl
    dl.below.above = dl
    dl.col.size += 1
    return dl
end

function uncover!(col)
    for node in Iterators.reverse(col)
        for elt in Iterators.reverse(RestRow(node))
            restore_ab!(elt)
        end
    end
    restore_rl!(links(col))
end

function insert_row!(root::LinkMatrix, col_ids)
    isempty(col_ids) && return
    num_inserted = 0
    first_in_row = nothing
    prevdl = nothing
    for col in root
        if id(col) in col_ids
            newdl = Link(col)
            if isnothing(first_in_row)
                first_in_row = newdl
            end
            first_in_row.left = newdl
            newdl.right = first_in_row
            if !isnothing(prevdl)
                prevdl.right = newdl
                newdl.left = prevdl
            end
            prevdl = newdl
            num_inserted += 1
        end
    end
    if num_inserted == length(col_ids)
        for node in FullRow(first_in_row)
            col = node.col
            collinks = links(col)
            lastdl = above(collinks)
            col.size += 1
            node.above = lastdl
            node.below = collinks
            lastdl.below = node
            collinks.above = node
        end
        return first_in_row
    else
        return
    end
end

function dl_cols(names)
    T = eltype(names)
    root = LinkMatrix{T}()
    rootlinks = columns(root)
    prev = rootlinks
    for name in names
        col = LinkColumn{T}(name)
        collinks = links(col)
        collinks.left = prev
        collinks.right = rootlinks
        prev.right = collinks
        rootlinks.left = collinks
        prev = collinks
    end
    return root
end

function find_col(root, search_id)
    for col in root
        id(col) == search_id && return col
    end
    return
end

end #module DancingLinks