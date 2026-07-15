package tim;

import java.math.BigDecimal;

public class Formats {
    public static Position position(int x, int y) {
        return new Position(x, y);
    }
    public static Direction direction(int right, int down) {
        return new Direction(right, down);
    }
    public static Format cell(CellParser parse) {
        return new Format.Cell(parse);
    }
    public static Format constant(String text) {
        return new Format.Constant(text);
    }
    public static Format record(Format.RecordConstructor constructor, Direction direction, Format... fieldFormats) {
        return new Format.Record(constructor, direction, fieldFormats);
    }
    public static Format list(Direction direction, Format elementFormat) {
        return new Format.List(direction, elementFormat);
    }
    public static Format text = cell(content ->
            switch (content) {
                case String text -> new StringValue(text);
                case BigDecimal decimal -> new StringValue(decimal.toString());
                default -> throw new RuntimeException("invalid cell contents: " + content.toString());
            });

    public static Format integer = cell(content ->
            switch (content) {
                case String text -> new IntValue(Integer.parseInt(text));
                case BigDecimal decimal -> new IntValue(decimal.intValue());
                default -> throw new RuntimeException("invalid cell contents: " + content.toString());
            });
    public static Format ignore(Direction direction, Format... formats) {
        return record(formats_ -> null, direction, formats);
    }
    public static Format choose(int index, Direction direction, Format...fieldFormats) {
        return record(values -> values[index], direction, fieldFormats);
    }
}
