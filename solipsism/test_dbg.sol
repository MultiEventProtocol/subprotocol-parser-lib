pragma solidity ^0.8.0;

library EnumerableMap {

    struct Map {
        MapEntry[] _entries;
        mapping (bytes32 => uint256) _indexes;
    }

    function _set(Map storage map, bytes32 key, bytes32 value) private returns (bool) {
        uint256 keyIndex = map._indexes[key];
        map._entries.push(MapEntry({ _key: key, _value: value }));
        map._indexes[key] = map._entries.length;
        map._entries[keyIndex - 1]._value = value;
    }
}
