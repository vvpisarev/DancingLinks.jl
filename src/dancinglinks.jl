module DancingLinks

using Base: @propagate_inbounds

export Cover, LinkMatrix, CoverRow, LinkColumn
export insert_row!, algorithm_x!, id

struct LinkRow
    above::Vector{Tuple{Int, Int}}
    below::Vector{Tuple{Int, Int}}
    col::Vector{Int}
    function LinkRow(nlinks::Integer=0)
        col = collect(1:nlinks)
        above = similar(col, Tuple{Int, Int})
        below = similar(col, Tuple{Int, Int})
        self = new(above, below, col)
        for i in eachindex(above, below)
            above[i] = below[i] = (0, i)
        end
        return self
    end
end

LinkRow(root::M, nlinks::Integer=0) where {M} = LinkRow{M}(root, nlinks)

struct LinkMatrix{T}
    id::Vector{T}
    prev::Vector{Int}
    next::Vector{Int}
    size::Vector{Int}
    rows::Vector{LinkRow}
    function LinkMatrix{T}(ids) where {T}
        id = collect(T, ids)
        ncols = length(id)
        vlink = LinkRow(ncols)
        prev = collect(-1:ncols-1)
        next = collect(1:ncols+1)
        prev[1] = ncols
        next[end] = 0
        size = zeros(Int, ncols)
        rows = [vlink]
        self = new{T}(id, prev, next, size, rows)
        return self
    end
end

struct Cover{M<:LinkMatrix}
    matr::M
    rows::Vector{Int}
end

struct CoverRow{M<:LinkMatrix}
    matr::M
    idx::Int
end

struct LinkColumn{M<:LinkMatrix}
    matr::M
    idx::Int
end

Base.push!(c::Cover, i::Integer) = push!(c.rows, i)
Base.pop!(c::Cover) = pop!(c.rows)

function Base.iterate(c::Cover, i::Integer=1)
    i in eachindex(c.rows) || return
    return CoverRow(c.matr, c.rows[i]), i+1
end

function Base.iterate(row::CoverRow, i::Integer=1)
    linkrow = row.matr.rows[row.idx+1]
    i in eachindex(linkrow.col) || return
    return id(row.matr, linkrow.col[i]), i+1
end

LinkMatrix(ids) = LinkMatrix{eltype(ids)}(ids)

Base.isempty(m::LinkMatrix) = first(m.next) < 1

@propagate_inbounds id(matr::LinkMatrix, col::Int) = matr.id[col]

@propagate_inbounds id(col::LinkColumn) = id(col.matr, col.idx)
@propagate_inbounds Base.length(col::LinkColumn) = col.matr.size[col.idx]

@propagate_inbounds function below(matr, row, col)
    searchrow = getrow(matr, row)
    return searchrow.below[col]
end

@propagate_inbounds function above(matr, row, col)
    searchrow = getrow(matr, row)
    return searchrow.above[col]
end

@propagate_inbounds function Base.iterate(matr::LinkMatrix, ind=matr.next[1])
    ind == 0 && return
    return LinkColumn(matr, ind), matr.next[ind]
end

@propagate_inbounds getrow(matr::LinkMatrix, irow::Integer) = matr.rows[irow+1]

function algorithm_x!(root::LinkMatrix, solution=Cover(root, Int[]))
    if isempty(root)
        return solution
    end
    @inbounds begin
        col = choose_col(root)
        cover!(root, col)
        nextrow, ind = below(root, 0, col)
        while nextrow != 0
            row = getrow(root, nextrow)
            push!(solution, nextrow)
            for j in row.col
                j == col || cover!(root, j)
            end
            if !isnothing(algorithm_x!(root, solution))
                return solution
            end
            row = getrow(root, pop!(solution))
            for j in Iterators.reverse(row.col)
                j == col || uncover!(root, j)
            end
            nextrow, ind = below(root, nextrow, ind)
        end
        uncover!(root, col)
        return
    end
end

@propagate_inbounds function choose_col(root)
    bestcol = first(root.next)
    s = root.size[bestcol]
    col = root.next[bestcol+1]
    inds = eachindex(root.size)
    @inbounds while col in inds
        l = root.size[col]
        if l < s
            s, bestcol = l, col
        end
        col = root.next[col+1]
    end
    return bestcol
end

@propagate_inbounds function detach_rl!(matr, col)
    next, prev = matr.next[col+1], matr.prev[col+1]
    matr.prev[next+1] = prev
    matr.next[prev+1] = next
    return col
end

@propagate_inbounds function detach_ab!(matr, irow, i)
    rabove, iabove = above(matr, irow, i)
    rbelow, ibelow = below(matr, irow, i)
    row_a = getrow(matr, rabove)
    row_b = getrow(matr, rbelow)
    row_a.below[iabove] = rbelow, ibelow
    row_b.above[ibelow] = rabove, iabove
    matr.size[getrow(matr, irow).col[i]] -= 1
end

@propagate_inbounds function cover!(matr, col)
    detach_rl!(matr, col)
    nextrow, ind = below(matr, 0, col)
    @inbounds while nextrow != 0
        row = getrow(matr, nextrow)
        for i in eachindex(row.col)
            i == ind || detach_ab!(matr, nextrow, i)
        end
        nextrow, ind = below(matr, nextrow, ind)
    end
    return col
end

@propagate_inbounds function restore_rl!(matr, col)
    next, prev = matr.next[col+1], matr.prev[col+1]
    matr.prev[next+1] = matr.next[prev+1] = col
    return col
end

@propagate_inbounds function restore_ab!(matr, irow, i)
    rabove, iabove = above(matr, irow, i)
    rbelow, ibelow = below(matr, irow, i)
    row_a = getrow(matr, rabove)
    row_b = getrow(matr, rbelow)
    row_a.below[iabove] = row_b.above[ibelow] = irow, i
    matr.size[getrow(matr, irow).col[i]] += 1
end

@propagate_inbounds function uncover!(matr, col)
    nextrow, ind = above(matr, 0, col)
    @inbounds while nextrow != 0
        row = getrow(matr, nextrow)
        for i in eachindex(row.above)
            i == ind || restore_ab!(matr, nextrow, i)
        end
        nextrow, ind = above(matr, nextrow, ind)
    end
    restore_rl!(matr, col)
    return col
end

function insert_row!(root::LinkMatrix, col_ids)
    isempty(col_ids) && return
    new_row = LinkRow(length(col_ids))
    nextind = 1
    id = root.id
    @inbounds for col in eachindex(id)
        if id[col] in col_ids
            new_row.col[nextind] = col
            nextind += 1
        end
        if nextind > lastindex(new_row.col)
            push!(root.rows, new_row)
            nnew = lastindex(root.rows)-1
            for i in eachindex(new_row.col)
                icol = new_row.col[i]
                lastrow, ilast = above(root, 0, icol)
                new_row.above[i] = lastrow, ilast
                new_row.below[i] = 0, icol
                lastrow_a = getrow(root, lastrow)
                lastrow_a.below[ilast] = getrow(root, 0).above[icol] = nnew, i
                root.size[icol] += 1
            end
            return new_row
        end
    end
    return
end

end #module DancingLinks