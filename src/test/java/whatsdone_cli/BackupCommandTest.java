package whatsdone_cli;

import static org.mockito.Mockito.*;
import org.junit.Test;
import org.mockito.Mockito;

public class BackupCommandTest {

    @Test
    public void test_it_loads_config_file() {
        DoneRepo doneRepo = Mockito.mock(DoneRepo.class);
        when(doneRepo.getAll()).thenReturn("BACKUP_DATA");
        BackupStore backupStore = Mockito.mock(BackupStore.class);
        
        BackupCommand command = new BackupCommand(doneRepo, backupStore);
        command.execute();
        
        verify(backupStore).backup("BACKUP_DATA");
    }

}
