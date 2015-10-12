package me.piebridge.billing;

import android.content.ComponentName;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.Handler;
import android.os.HandlerThread;
import android.os.IBinder;
import android.os.RemoteException;

import com.android.vending.billing.IInAppBillingService;

import java.util.List;

import me.piebridge.prevent.ui.UILog;

/**
 * Created by thom on 15/10/11.
 */
public class DonateService implements ServiceConnection {

    private IInAppBillingService mService;

    private final Handler mHandler;

    private final String mPackageName;

    private final DonateActivity mDonateActivity;

    public DonateService(DonateActivity donateActivity) {
        mDonateActivity = donateActivity;
        mPackageName = donateActivity.getPackageName();
        HandlerThread thread = new HandlerThread("DonateService");
        thread.start();
        mHandler = new Handler(thread.getLooper());
    }

    @Override
    public void onServiceConnected(ComponentName name, IBinder service) {
        mService = IInAppBillingService.Stub.asInterface(service);
        mHandler.post(new Runnable() {
            @Override
            public void run() {
                if (!isBillingSupported()) {
                    UILog.e("billing is not supported");
                    onUnavailable(mService);
                } else {
                    onAvailable(mService);
                }
                mDonateActivity.unbindService(DonateService.this);
            }
        });
    }

    protected void onAvailable(IInAppBillingService service) {
        mDonateActivity.onAvailable(service);
    }

    protected void onUnavailable(IInAppBillingService service) {
        mDonateActivity.onUnavailable(service);
    }

    @Override
    public void onServiceDisconnected(ComponentName name) {
        mService = null;
    }

    protected boolean isBillingSupported() {
        try {
            return mService.isBillingSupported(DonateUtils.API_VERSION, mPackageName, DonateUtils.ITEM_TYPE) == 0;
        } catch (RemoteException e) {
            UILog.d("cannot checking billing supported", e);
            return false;
        }
    }

    protected boolean isDonated() {
        try {
            Bundle bundle = mService.getPurchases(DonateUtils.API_VERSION, mPackageName, DonateUtils.ITEM_TYPE, null);
            return bundle != null && bundle.getInt("RESPONSE_CODE") == 0 && isPurchased(bundle);
        } catch (RemoteException e) {
            UILog.d("cannot get purchases", e);
            return false;
        }
    }

    protected boolean isPurchased(Bundle bundle) {
        List<String> dataList = bundle.getStringArrayList("INAPP_PURCHASE_DATA_LIST");
        List<String> signatureList = bundle.getStringArrayList("INAPP_DATA_SIGNATURE_LIST");

        if (DonateUtils.isEmpty(dataList) || DonateUtils.isEmpty(signatureList)) {
            return false;
        }

        int size = dataList.size();
        if (size > signatureList.size()) {
            size = signatureList.size();
        }

        for (int i = 0; i < size; ++i) {
            if (DonateUtils.verify(dataList.get(i), signatureList.get(i))) {
                return true;
            }
        }

        return false;
    }

}
