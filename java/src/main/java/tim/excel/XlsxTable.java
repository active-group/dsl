package tim.excel;

import java.io.FileInputStream;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.List;

import org.dhatim.fastexcel.reader.ReadableWorkbook;
import org.dhatim.fastexcel.reader.Row;

public class XlsxTable implements tim.Table, AutoCloseable {
    private final ReadableWorkbook workbook;
    private final List<Row> sheetRows;

    private XlsxTable(String path, String worksheetName) throws IOException {
        var stream = new FileInputStream(path);
        this.workbook = new ReadableWorkbook(stream);
        var optSheet = this.workbook.findSheet(worksheetName);
        if (optSheet.isPresent()) {
            this.sheetRows = optSheet.get().read();
        } else {
            throw new IOException(MessageFormat.format("Worksheet {0} not found.", worksheetName));
        }
    }

    public static XlsxTable fromFile(String path, String worksheetName) throws IOException {
        return new XlsxTable(path, worksheetName);
    }

    @Override
    public Object ref(int x, int y) {
        if (y >= this.sheetRows.size()) {
            return null;
        }

        var row = this.sheetRows.get(y);
        if (x >= row.getCellCount()) {
            return null;
        }

        var cell = row.getCell(x);
        return switch (cell.getType()) {
            case NUMBER -> cell.asNumber();
            case BOOLEAN -> cell.asBoolean();
            case STRING -> cell.asString();
            // FIXME
            case FORMULA -> null;
            case ERROR -> null;
            case EMPTY -> null;
        };
    }

    @Override
    public void close() throws Exception {
        this.workbook.close();
    }
}