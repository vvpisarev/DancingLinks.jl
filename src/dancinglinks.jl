module DancingLinks

using Base: @propagate_inbounds

export Cover, LinkMatrix, CoverRow
export insert_row!, algorithm_x!

mutable struct Link{R}
    above::Link{R}
    below::Link{R}
    row::R
    col::Int
    function Link{R}() where R
        self = new{R}()
        self.above = self.below = self
        self.col = 0
        return self
    end    
    function Link{R}(row) where R
        self = new{R}()
        self.above = self.below = self
        self.row = row
        self.col = 0
        return self
    end
end

Link(row::R) where R = Link{R}(row)

struct LinkRow
    links::Vector{Link{LinkRow}}
    function LinkRow(nlinks::Integer=0)
        links = [Link{LinkRow}() for _ in 1:nlinks]
        self = new(links)
        for (i,link) in enumerate(links)
            link.row = self
            link.col = i
        end
        return self
    end
end

@propagate_inbounds Base.getindex(r::LinkRow, i::Integer) = r.links[i]
@propagate_inbounds Base.setindex!(r::LinkRow, val, i::Integer) = setindex!(r.links, val, i)
Base.eachindex(r::LinkRow) = eachindex(r.links)
Base.firstindex(r::LinkRow) = firstindex(r.links)
Base.lastindex(r::LinkRow) = lastindex(r.links)

function Base.iterate(row::LinkRow, i::Integer=1)
    i in eachindex(row) || return
    @inbounds return row[i], i+1
end

function Base.iterate(rrow::Iterators.Reverse{LinkRow}, i::Integer=lastindex(rrow.itr))
    row = rrow.itr
    i in eachindex(row) || return
    @inbounds return row[i], i-1
end

struct LinkMatrix{T}
    id::Vector{T}
    prev::Vector{Int}
    next::Vector{Int}
    size::Vector{Int}
    vlink::LinkRow
    function LinkMatrix{T}(ids) where {T}
        id = collect(T, ids)
        ncols = length(id)
        vlink = LinkRow(ncols)
        prev = collect(-1:ncols-1)
        next = collect(1:ncols+1)
        prev[1] = ncols
        next[end] = 0
        size = zeros(Int, ncols)
        self = new{T}(id, prev, next, size, vlink)
        return self
    end
end

LinkMatrix(ids) = LinkMatrix{eltype(ids)}(ids)

Base.isempty(m::LinkMatrix) = first(m.next) < 1

struct Cover{M<:LinkMatrix}
    matr::M
    rows::Vector{LinkRow}
end

Cover(matr::LinkMatrix) = Cover(matr, LinkRow[])

struct CoverRow{M<:LinkMatrix}
    matr::M
    lrow::LinkRow
end

Base.push!(c::Cover, row::LinkRow) = push!(c.rows, row)
Base.pop!(c::Cover) = pop!(c.rows)

function Base.iterate(c::Cover, i::Integer=1)
    i in eachindex(c.rows) || return
    return CoverRow(c.matr, c.rows[i]), i+1
end

function Base.iterate(row::CoverRow, i::Integer=1)
    linkrow = row.lrow
    i in eachindex(linkrow) || return
    return id(row.matr, linkrow[i].col), i+1
end

@propagate_inbounds id(matr::LinkMatrix, col::Int) = matr.id[col]

function algorithm_x!(root::LinkMatrix, solution=Cover(root))
    if isempty(root)
        return solution
    end
    @inbounds begin
        col = choose_col(root)
        cover!(root, col)
        collink = root.vlink[col]
        nextlink = collink.below
        while nextlink !== collink
            row = nextlink.row
            push!(solution, row)
            for link in row.links
                link.col == col || cover!(root, link.col)
            end
            if !isnothing(algorithm_x!(root, solution))
                return solution
            end
            row = pop!(solution)
            for link in Iterators.reverse(row.links)
                link.col == col || uncover!(root, link.col)
            end
            nextlink = nextlink.below
        end
        uncover!(root, col)
        return
    end
end

@propagate_inbounds function choose_col(root)
    bestcol = first(root.next)
    s = root.size[bestcol]
    s < 2 && return bestcol
    col = root.next[bestcol+1]
    inds = eachindex(root.size)
    @inbounds while col in inds
        l = root.size[col]
        if l < 2
            return col
        elseif l < s
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

@propagate_inbounds function detach_ab!(matr, link)
    link.below.above = link.above
    link.above.below = link.below
    matr.size[link.col] -= 1
end

@propagate_inbounds function cover!(matr, col)
    detach_rl!(matr, col)
    collink = matr.vlink[col]
    nextlink = collink.below
    @inbounds while nextlink !== collink
        row = nextlink.row
        for link in row.links
            link.col == col || detach_ab!(matr, link)
        end
        nextlink = nextlink.below
    end
    return col
end

@propagate_inbounds function restore_rl!(matr, col)
    next, prev = matr.next[col+1], matr.prev[col+1]
    matr.prev[next+1] = matr.next[prev+1] = col
    return col
end

@propagate_inbounds function restore_ab!(matr, link)
    link.below.above = link.above.below = link
    matr.size[link.col] += 1
end

@propagate_inbounds function uncover!(matr, col)
    collink = matr.vlink[col]
    nextlink = collink.above
    @inbounds while nextlink !== collink
        row = nextlink.row
        for link in row.links
            link.col == col || restore_ab!(matr, link)
        end
        nextlink = nextlink.above
    end
    restore_rl!(matr, col)
    return col
end

function insert_row!(root::LinkMatrix, col_ids)
    isempty(col_ids) && return
    new_row = LinkRow(length(col_ids))
    links = new_row.links
    nextind = 1
    id = root.id
    @inbounds for col in eachindex(id)
        if id[col] in col_ids
            links[nextind].col = col
            nextind += 1
        end
        if nextind > lastindex(links)
            for link in links
                icol = link.col
                collink = root.vlink[icol]
                link.below = collink
                link.above = collink.above
                link.above.below = collink.above = link
                root.size[icol] += 1
            end
            return new_row
        end
    end
    return
end

end #module DancingLinks