package tim;

import org.junit.jupiter.api.Test;
import tim.excel.XlsxTable;

import java.io.IOException;
import java.text.MessageFormat;
import java.util.List;

import static org.assertj.core.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class ParseTest {
    @Test
    void formatInteger() {
        var result = Formats.integer.parse((x, y) -> "123", Position.of(0, 0));
        assertThat(result).isEqualTo(IntValue.of(123));
    }


    @Test
    void formatConstant() {
        var format = Formats.constant("Mike");
        assertThat(format.parse((x, y) -> "Mike", Position.of(0, 0)))
                .isEqualTo(StringValue.of("Mike"));
        assertThrows(RuntimeException.class, () ->
                format.parse((x, y) -> "Kaan", Position.of(0, 0)));
    }

    record Entry(String name, String address, int phone) implements Value {
        static Value constructor(Value... args) {
            return new Entry(args[0].getString(),
                    args[1].getString(),
                    args[2].getInt());
        }
    }

    @Test
    void formatRecord() {
        String[][] contents = {{"Mike", "Pappelweg", "123"}};
        var format = Formats.record(Entry::constructor, Directions.right,
                Formats.text, Formats.text, Formats.integer);
        assertThat(format.parse(Table.fromArray(contents), Position.of(0, 0)))
                .isEqualTo(new Entry("Mike", "Pappelweg", 123));
    }

    @Test
    void listFormat() {
        String[][] contents = {{"1", "2", "3"}};

        var format = Formats.list(Directions.right, Formats.integer);
        assertThat(format.parse(Table.fromArray(contents), Position.of(0, 0)))
                .isEqualTo(ArrayValue.of(IntValue.of(1), IntValue.of(2), IntValue.of(3)));
    }

    @Test
    void realFormat() {
        var format =
                Formats.choose(1, Directions.down,
                        Formats.ignore(Directions.right,
                                Formats.constant("Name"),
                                Formats.constant("Address"),
                                Formats.constant("Phone")),
                        Formats.list(Directions.down,
                                Formats.record(Entry::constructor, Directions.right,
                                        Formats.text, Formats.text, Formats.integer)));
        String[][] contents = {{"Name", "Address", "Phone"},
                {"Mike", "Pappelweg", "162"},
                {"Kaan", "Hannover", "511"}};
        assertThat(format.parse(Table.fromArray(contents), Position.of(0, 0)))
                .isEqualTo(ArrayValue.of(new Entry("Mike", "Pappelweg", 162),
                        new Entry("Kaan", "Hannover", 511)));
    }

    @Test
    void realFormatWithRealExcelSheet() {
        var format =
                Formats.choose(1, Directions.down,
                        Formats.ignore(Directions.right,
                                Formats.constant("Name"),
                                Formats.constant("Address"),
                                Formats.constant("Phone")),
                        Formats.list(Directions.down,
                                Formats.record(Entry::constructor, Directions.right,
                                        Formats.text, Formats.text, Formats.integer)));
        try {
            assertThat(format.parse(XlsxTable.fromFile("../racket/tim/read.xlsx", "Addresses"), Position.of(0,0)))
                    .isEqualTo(ArrayValue.of(new Entry("Mike", "Tübingen", 123),
                                             new Entry("Kaan", "Hannover", 987)));
        } catch (IOException e) {
            var msg = MessageFormat.format("Unexpected exception: {0}", e.toString());
            fail(msg);
        }
    }

    record Person(String name, int age) implements Value {
        static Value constructor(Value... args) {
            return new Person(args[0].getString(), args[1].getInt());
        }
    }

    record PersonData(int id, Person person) implements Value {
        static Value constructor(Value... args) {
            return new PersonData(args[0].getInt(), (Person)args[1]);
        }
    }

    @Test
    void nestedRecordFormats() {
        var format =
                Formats.list(
                        Directions.down,
                        Formats.record(
                                PersonData::constructor,
                                Directions.right,
                                Formats.integer,
                                Formats.record(Person::constructor, Directions.right, Formats.text, Formats.integer)));
        String[][] contents = {{ "1", "Johannes", "37" }, { "2", "Frederik", "7"}};
        var result = format.parse(Table.fromArray(contents), Position.of(0, 0));
        assertThat(result).isEqualTo(ArrayValue.of(
                new PersonData(1, new Person("Johannes", 37)),
                new PersonData(2, new Person("Frederik", 7))));
    }
}