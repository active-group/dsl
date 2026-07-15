package tim;

import java.util.ArrayList;

public sealed interface Format extends Value {
    record Cell(CellParser parser) implements Format {}
    record Constant(String text) implements Format {}

    interface RecordConstructor {
        Value construct(Value... args);
    }
    record Record(RecordConstructor constructor, Direction direction,
                  Format[] fieldFormats)
            implements Format {}
    record List(Direction direction, Format elementFormat)
            implements Format
    {}

    default Value parse(Table table, Position position) {
        return switch (this) {
            case Cell(CellParser parser) ->
                    parser.parse(table.ref(position));
            case Constant(String text) -> {
                   Object contents = table.ref(position);
                   if (contents.toString().equals(text))
                       yield new StringValue(contents.toString());
                   else
                       throw new RuntimeException("found contents " + contents.toString()
                       + ", expected " + text);
            }
            case Record(RecordConstructor constructor, Direction direction,
                        Format[] fieldFormats) -> {
                var parseds = new ArrayList<Value>();
                for (Format fieldFormat : fieldFormats) {
                    parseds.add(fieldFormat.parse(table, position));
                    position = position.move(direction);
                }
                yield constructor.construct(parseds.toArray(new Value[0]));
            }
            case List(Direction direction, Format elementFormat) -> {
                var parseds = new ArrayList<Value>();
                while (table.ref(position) != null) {
                    parseds.add(elementFormat.parse(table, position));
                    position = position.move(direction);
                }
                yield new ArrayValue(parseds.toArray(new Value[0]));
            }
        };
    }
}

