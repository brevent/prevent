package me.piebridge.prevent.framework;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.accounts.AuthenticatorDescription;
import android.accounts.OnAccountsUpdateListener;
import android.content.Context;
import android.os.Handler;
import android.os.HandlerThread;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Created by thom on 15/8/7.
 */
public class AccountWatcher implements OnAccountsUpdateListener {

    private static Map<String, String> types = new HashMap<String, String>();

    private static Set<String> mEnabledPackages = new HashSet<String>();

    private final AccountManager accountManager;

    public AccountWatcher(Context context) {
        accountManager = AccountManager.get(context);
        accountManager.addOnAccountsUpdatedListener(this, initHandler(), true);
        updateAuthDescriptions();
    }

    private Handler initHandler() {
        HandlerThread thread = new HandlerThread("AccountWatcher");
        thread.start();
        return new Handler(thread.getLooper());
    }

    public void updateAuthDescriptions() {
        types.clear();
        for (AuthenticatorDescription ad : accountManager.getAuthenticatorTypes()) {
            if (ad.packageName != null) {
                types.put(ad.type, ad.packageName);
            }
        }
    }

    @Override
    public void onAccountsUpdated(Account[] accounts) {
        Account[] updatedAccounts;
        if (accounts == null) {
            updatedAccounts = accountManager.getAccounts();
        } else {
            updatedAccounts = accounts;
        }
        mEnabledPackages.clear();
        for (Account account : updatedAccounts) {
            String packageName = types.get(account.type);
            PreventLog.d("onAccountUpdated, type: " + account.type + ", packageName: " + packageName);
            if (packageName != null && !mEnabledPackages.contains(packageName)) {
                mEnabledPackages.add(packageName);
            }
        }
        PreventLog.d("enabled accounts: " + mEnabledPackages);
    }

    public boolean cannotHook(String action, String packageName) {
        return AccountManager.ACTION_AUTHENTICATOR_INTENT.equals(action) && mEnabledPackages.contains(packageName);
    }

    public static boolean isEnabled(String packageName) {
        return mEnabledPackages != null && mEnabledPackages.contains(packageName);
    }

}
