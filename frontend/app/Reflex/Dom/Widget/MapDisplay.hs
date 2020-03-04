module Reflex.Dom.Widget.MapDisplay where

data MapDisplay t k = MapDisplay {
                    _mapDisplayItemClicked :: Event t k
                    }
