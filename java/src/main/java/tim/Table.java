package tim;

@FunctionalInterface
public interface Table {
    Object ref(int x, int y);
    default Object ref(Position position) {
        return ref(position.x(), position.y());
    }

    static Table fromArray(String[][] contents) {
        return (x, y) -> {
            try {
                return contents[y][x];
            } catch (ArrayIndexOutOfBoundsException e) {
                return null;
            }
        };
    }
}