package whatsdone_cli;

public class BackupCommand {

    private final DoneRepo doneRepo;
    private final BackupStore backupStore;
    
    BackupCommand(DoneRepo doneRepo, BackupStore backupStore) {
        this.backupStore = backupStore;
        this.doneRepo = doneRepo;
    }
    
    void execute() {
        backupStore.backup(doneRepo.getAll());
    }

}
