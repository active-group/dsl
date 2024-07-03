#lang tim
// address entry
defrecord entry { name, address, phone };

right = #(1, 0);
down = #(0, 1);

entry-row = record entry (right) { text, text, number };
entries = list (down) entry-row;
heading = ignore (right) { "Name", "Address", "Phone" };
addressbook = choose 2 (down) { heading, entries };

table addressbook @(0, 0)
