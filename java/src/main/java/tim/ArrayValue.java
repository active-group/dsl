package tim;

import java.sql.Array;
import java.util.Arrays;

public record ArrayValue(Value[] values) implements Value {
    public static Object of(Value... values) {
        return new ArrayValue(values);
    }

    @Override
    public boolean equals(Object obj) {
        return switch (obj) {
            case ArrayValue(Value[] otherValues) -> Arrays.equals(values, otherValues);
            default -> false;
        };
    }
}