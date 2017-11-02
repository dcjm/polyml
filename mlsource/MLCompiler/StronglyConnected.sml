(*
    Copyright (c) 2016-17 David C.J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

structure StronglyConnected:
sig
    val stronglyConnectedComponents: {nodeAddress: 'a -> int, arcs: 'a -> int list } -> 'a list -> 'a list list
end
=
struct
    fun stronglyConnectedComponents _ [] = []
    |   stronglyConnectedComponents {nodeAddress, arcs} (rlist as firstNode :: _) =
    (* In general any mutually recursive declaration can refer to any
       other.  It's better to partition the recursive declarations into
       strongly connected components i.e. those that actually refer
       to each other.  *)
    let
        local
            val anAddr = nodeAddress firstNode
        in
            val (startAddress, lastAddress) =
                List.foldl (fn(item, (mn, mx)) => let val addr = nodeAddress item in (Int.min(addr, mn), Int.max(addr+1, mx)) end) (anAddr, anAddr) rlist
        end
        (* *)
        val mapArray = Array.array(lastAddress - startAddress, NONE)
        
        fun updateMin(addr, try) =
        let
            val off = addr - startAddress
            val { lowLink, index } = valOf(Array.sub(mapArray, off))
        in
            Array.update(mapArray, off, SOME{ index = index, lowLink = Int.min(lowLink, try) })
        end

        fun addrInList a = List.exists(fn item => a = nodeAddress item)

        fun strongcomponent(item, (thisIndex, stack, resList)) =
        let
            val addr = nodeAddress item
            val allArcs = arcs item
            val newStack = item :: stack
            val v = addr - startAddress
            (* Mark this item as processed. *)
            val () = Array.update(mapArray, v, SOME{index = thisIndex, lowLink = thisIndex})

            (* Process links that refer to other items *)
            fun processLink(a: int, args as (_, stack, _)) =
                    if addrInList a rlist
                    then (* It refers to another within this declaration *)
                    let
                        val w = a - startAddress
                    in
                        case Array.sub(mapArray, w) of
                            NONE => (*  Not yet processed. *)
                            let
                                val result = strongcomponent(valOf(List.find(fn item => nodeAddress item = a) rlist), args);
                            in
                                updateMin(addr, #lowLink(valOf(Array.sub(mapArray, w))));
                                result
                            end
                        |   SOME _ =>
                            (
                                (* Already processed - was it in this pass or a previous? *)
                                if addrInList a stack (* On the stack so in the current SCC *)
                                then updateMin(addr, #index(valOf(Array.sub(mapArray, w))))
                                else (); (* Processed in previous pass *)
                                args
                            )
                    end
                    else args
            
            val (nextIndex, stack', subRes) = List.foldl processLink (thisIndex+1, newStack, resList) allArcs
        in
            (* Process references from this function. *)
            if #lowLink(valOf(Array.sub(mapArray, v))) = thisIndex (* This is the minimum *)
            then (* Create an SCC *)
            let
                fun popItems([], _) = raise Fail "stack empty"
                |   popItems(item :: r, l) =
                        if nodeAddress item = addr
                        then (r, item :: l)
                        else popItems(r, item :: l)
                val (newStack, scc) = popItems(stack', [])
            in
                (nextIndex, newStack,  scc :: subRes)
            end
            else (nextIndex, stack', subRes)
        end

        (* Process items that have not yet been reached *)
        fun processUnprocessed (item, args) =
            case Array.sub(mapArray, nodeAddress item-startAddress) of 
                NONE => strongcomponent(item, args)
            |   _ => args

        val (_, _, result) = List.foldl processUnprocessed (0, [], []) rlist
    in
        result
    end
end;
