package me.piebridge.forcestopgb;

import java.text.Collator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import android.app.AlertDialog;
import android.app.ProgressDialog;
import android.app.ActivityManager.RunningAppProcessInfo;
import android.content.ActivityNotFoundException;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.app.ListFragment;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView.AdapterContextMenuInfo;
import android.widget.ArrayAdapter;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;

public abstract class XposedListFragment extends ListFragment {

	private Adapter mAdapter;
	private Locale prevLocale;
	private XposedActivity mActivity;
	private Set<String> prevNames = null;

	@Override
	public void onActivityCreated(Bundle savedInstanceState) {
		super.onActivityCreated(savedInstanceState);
		registerForContextMenu(getListView());
		mActivity = (XposedActivity) getActivity();
		if (mActivity != null) {
			setNewAdapterIfNeeded(mActivity, true);
		}
	}

	@Override
	public void onDestroyView() {
		super.onDestroyView();
		mActivity = null;
		setListAdapter(null);
	}

	@Override
	public void onListItemClick(ListView l, View v, int position, long id) {
		l.showContextMenuForChild(v);
	}

	@Override
	public void onCreateContextMenu(ContextMenu menu, View v, ContextMenuInfo menuInfo) {
		if (mActivity == null || menu == null || menuInfo == null) {
			return;
		}
		menu.clear();
		ViewHolder holder = (ViewHolder) ((AdapterContextMenuInfo) menuInfo).targetView.getTag();
		menu.setHeaderTitle(holder.nameView.getText());
		if (holder.icon != null) {
			menu.setHeaderIcon(holder.icon);
		}
		menu.add(Menu.NONE, R.string.app_info, Menu.NONE, R.string.app_info);
		if (mActivity.getPreventPackages().containsKey(holder.packageName)) {
			menu.add(Menu.NONE, R.string.remove, Menu.NONE, R.string.remove);
		} else {
			menu.add(Menu.NONE, R.string.prevent, Menu.NONE, R.string.prevent);
		}
		menu.add(Menu.NONE, R.string.uninstall, Menu.NONE, R.string.uninstall);
	}

	@Override
	public boolean onContextItemSelected(MenuItem item) {
		if (mActivity == null || item == null) {
			return false;
		}
		ViewHolder holder = (ViewHolder) ((AdapterContextMenuInfo) item.getMenuInfo()).targetView.getTag();
		String packageName = holder.packageName;
		switch (item.getItemId()) {
		case R.string.app_info:
			startActivity(new Intent(android.provider.Settings.ACTION_APPLICATION_DETAILS_SETTINGS, Uri.fromParts("package", packageName, null)));
			return true;
		case R.string.remove:
			holder.preventView.setVisibility(View.GONE);
			mActivity.changePrevent(packageName, false);
			return true;
		case R.string.prevent:
			holder.preventView.setVisibility(View.VISIBLE);
			holder.preventView.setImageResource(holder.running != null ? R.drawable.ic_menu_stop : R.drawable.ic_menu_block);
			mActivity.changePrevent(packageName, true);
			return true;
		case R.string.uninstall:
			mActivity.startActivity(new Intent(Intent.ACTION_DELETE, Uri.fromParts("package", packageName, null)));
			return true;
		default:
			return false;
		}
	}

	@Override
	public void onResume() {
		super.onResume();
		Position position = getListPosition();
		if (position != null) {
			getListView().setSelectionFromTop(position.position, position.top);
		}
	}

	@Override
	public void onPause() {
		saveListPosition();
		super.onPause();
	}

	public void saveListPosition() {
		ListView l = getListView();
		int position = l.getFirstVisiblePosition();
		View v = l.getChildAt(0);
		int top = (v == null) ? 0 : v.getTop();
		setListPosition(new Position(position, top));
	}

	public void refresh(boolean force) {
		if (mActivity != null) {
			setNewAdapterIfNeeded(mActivity, force);
		}
	}

	protected abstract Set<String> getPackageNames(XposedActivity activity);

	protected abstract boolean canUseCache();

	protected abstract void setListPosition(Position position);

	protected abstract Position getListPosition();

	private void setNewAdapterIfNeeded(XposedActivity activity, boolean force) {
		Set<String> names;
		if (force || prevNames == null) {
			names = getPackageNames(activity);
		} else {
			names = prevNames;
		}
		if (mAdapter == null || !Locale.getDefault().equals(prevLocale) || !names.equals(prevNames)) {
			if (mAdapter != null) {
				setListAdapter(null);
			}
			mAdapter = new Adapter(activity, names, canUseCache());
			setListAdapter(mAdapter);
			if (prevNames == null) {
				prevNames = new HashSet<String>();
			}
			prevNames.clear();
			prevNames.addAll(names);
			prevLocale = Locale.getDefault();
		} else {
			mAdapter.notifyDataSetChanged();
			Position position = getListPosition();
			if (position != null) {
				getListView().setSelectionFromTop(position.position, position.top);
			}
		}
	}

	static class Position {
		int position;
		int top;

		public Position(int _position, int _top) {
			position = _position;
			top = _top;
		}
	}

	static class AppInfo implements Comparable<AppInfo> {
		int flags;
		Drawable icon;
		String name = "";
		String packageName = "";
		Set<Integer> running = null;

		public AppInfo(String _packageName, String _name, Set<Integer> _running) {
			super();
			packageName = _packageName;
			if (_name != null) {
				name = _name;
			}
			running = _running;
		}

		@Override
		public String toString() {
			return (running == null ? "1" : "0") + (isSystem() ? "1" : "0") + "/" + name + "/" + packageName;
		}

		@Override
		public int compareTo(AppInfo another) {
			return Collator.getInstance().compare(toString(), another.toString());
		}

		public AppInfo flags(int _flags) {
			flags = _flags;
			return this;
		}

		public boolean isSystem() {
			return (flags & (ApplicationInfo.FLAG_SYSTEM | ApplicationInfo.FLAG_UPDATED_SYSTEM_APP)) != 0;
		}
	}

	static class ViewHolder {
		boolean check;
		String label;
		String packageName;
		CheckBox checkView;
		ImageView iconView;
		TextView nameView;
		TextView summaryView;
		TextView loadingView;
		ImageView preventView;
		Drawable icon;
		Set<Integer> running;
	}

	static class Adapter extends ArrayAdapter<AppInfo> {
		private PackageManager pm;
		private LayoutInflater inflater;
		private XposedActivity mActivity;
		private static Map<String, String> labels = new HashMap<String, String>();

		public Adapter(XposedActivity activity) {
			super(activity, R.layout.item);
			mActivity = activity;
			pm = mActivity.getPackageManager();
			inflater = LayoutInflater.from(activity);
		}

		public Adapter(final XposedActivity activity, Set<String> names, boolean cache) {
			this(activity);
			if (!cache && !XposedActivity.isXposedEnabled()) {
				// @formatter:off
				new AlertDialog.Builder(activity)
					.setTitle(R.string.app_name)
					.setMessage(R.string.app_notenabled)
					.setIcon(R.drawable.ic_launcher)
					.setPositiveButton(activity.getString(android.R.string.ok), new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog, int which) {
							Intent intent= new Intent("de.robv.android.xposed.installer.OPEN_SECTION");
							intent.setPackage("de.robv.android.xposed.installer");
							intent.putExtra("section", 1);
							intent.putExtra("opentab", 1);
							intent.putExtra("module", mActivity.getPackageName());
							intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
							try {
								activity.startActivity(intent);
							} catch (ActivityNotFoundException e) {
								intent = activity.getPackageManager().getLaunchIntentForPackage("de.robv.android.xposed.installer");
								if (intent != null) {
									activity.startActivity(intent);
								} else {
									activity.finish();
								}
							}
						}
					}).create().show();
				// @formatter:on
			} else {
				addAll(names, cache);
			}
		}

		public void addAll(final Set<String> names, final boolean cache) {
			new AsyncTask<Void, Integer, TreeSet<AppInfo>>() {
				ProgressDialog dialog;

				@Override
				protected void onPreExecute() {
					if (!cache) {
						labels.clear();
						dialog = new ProgressDialog(mActivity);
						dialog.setMessage(mActivity.getString(R.string.loading));
						dialog.setProgressStyle(ProgressDialog.STYLE_HORIZONTAL);
						dialog.setCancelable(false);
						dialog.setMax(names.size());
						dialog.show();
					}
				}

				@Override
				protected TreeSet<AppInfo> doInBackground(Void... params) {
					Map<String, Set<Integer>> running = mActivity.getRunningProcesses();
					TreeSet<AppInfo> applications = new TreeSet<AppInfo>();
					int i = 1;
					for (String name : names) {
						try {
							publishProgress(++i);
							ApplicationInfo info = pm.getApplicationInfo(name, 0);
							if (!info.enabled) {
								continue;
							}
							String label;
							if (!cache) {
								label = info.loadLabel(pm).toString();
								labels.put(name, label);
							} else {
								label = labels.get(name);
								if (label == null) {
									label = info.loadLabel(pm).toString();
								}
							}
							applications.add(new AppInfo(name, label, running.get(name)).flags(info.flags));
						} catch (NameNotFoundException e) {
							e.printStackTrace();
						}
					}
					return applications;
				}

				@Override
				protected void onProgressUpdate(Integer... progress) {
					if (!cache) {
						dialog.setProgress(progress[0]);
					}
				}

				@Override
				protected void onPostExecute(TreeSet<AppInfo> applications) {
					for (AppInfo application : applications) {
						add(application);
					}
					try {
						if (!cache) {
							dialog.dismiss();
						}
					} catch (Exception e) {
					}
				}
			}.execute();
		}

		@Override
		public View getView(int position, View convertView, ViewGroup parent) {
			View view = convertView;
			if (view == null) {
				view = inflater.inflate(R.layout.item, null, true);
				ViewHolder viewHolder = new ViewHolder();
				viewHolder.checkView = (CheckBox) view.findViewById(R.id.check);
				viewHolder.iconView = (ImageView) view.findViewById(R.id.icon);
				viewHolder.nameView = (TextView) view.findViewById(R.id.name);
				viewHolder.summaryView = (TextView) view.findViewById(R.id.summary);
				viewHolder.loadingView = (TextView) view.findViewById(R.id.loading);
				viewHolder.preventView = (ImageView) view.findViewById(R.id.prevent);
				viewHolder.checkView.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
					@Override
					public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
						ViewHolder holder = (ViewHolder) buttonView.getTag();
						Set<String> selections = mActivity.getSelection();
						if (isChecked) {
							selections.add(holder.packageName);
						} else {
							selections.remove(holder.packageName);
						}
						mActivity.checkSelection();
					}
				});
				viewHolder.checkView.setTag(viewHolder);
				view.setTag(viewHolder);
			}

			ViewHolder holder = (ViewHolder) view.getTag();
			AppInfo appInfo = getItem(position);
			holder.label = appInfo.name;
			holder.packageName = appInfo.packageName;
			holder.nameView.setText(appInfo.name);
			holder.summaryView.setVisibility(View.GONE);
			holder.loadingView.setVisibility(View.VISIBLE);
			holder.checkView.setChecked(mActivity.getSelection().contains(holder.packageName));
			Boolean result = mActivity.getPreventPackages().get(appInfo.packageName);
			if (appInfo.isSystem()) {
				view.setBackgroundColor(mActivity.getThemedColor(R.attr.color_dangerous));
			} else {
				view.setBackgroundColor(mActivity.getColor(android.R.color.transparent));
			}
			if (result == null) {
				holder.preventView.setVisibility(View.INVISIBLE);
			} else {
				holder.preventView.setVisibility(View.VISIBLE);
				holder.preventView.setImageResource(result ? R.drawable.ic_menu_block : R.drawable.ic_menu_stop);
			}
			new AsyncTask<Object, Void, ViewHolder>() {
				@Override
				protected ViewHolder doInBackground(Object... params) {
					ViewHolder holder = (ViewHolder) params[0];
					AppInfo appInfo = (AppInfo) params[1];
					if (appInfo.icon == null) {
						try {
							appInfo.icon = ((PackageManager) params[2]).getApplicationIcon(appInfo.packageName);
						} catch (NameNotFoundException e) {
						}
					}
					holder.icon = appInfo.icon;
					holder.running = mActivity.getRunningProcesses().get(appInfo.packageName);
					return holder;
				}

				@Override
				protected void onPostExecute(ViewHolder holder) {
					holder.iconView.setImageDrawable(holder.icon);
					holder.loadingView.setVisibility(View.GONE);
					holder.summaryView.setVisibility(View.VISIBLE);
					holder.summaryView.setText(formatRunning(holder.running));
				}
			}.execute(holder, appInfo, pm);
			return view;
		}

		private CharSequence formatRunning(Set<Integer> running) {
			if (running == null) {
				return mActivity.getString(R.string.notrunning);
			} else {
				TreeSet<String> sets = new TreeSet<String>();
				for (Integer i : running) {
					switch (i) {
					case RunningAppProcessInfo.IMPORTANCE_BACKGROUND:
						sets.add(mActivity.getString(R.string.background));
						break;
					case RunningAppProcessInfo.IMPORTANCE_EMPTY:
						sets.add(mActivity.getString(R.string.empty));
						break;
					case RunningAppProcessInfo.IMPORTANCE_FOREGROUND:
						sets.add(mActivity.getString(R.string.foreground));
						break;
					case RunningAppProcessInfo.IMPORTANCE_PERCEPTIBLE:
						sets.add(mActivity.getString(R.string.perceptible));
						break;
					case RunningAppProcessInfo.IMPORTANCE_SERVICE:
						sets.add(mActivity.getString(R.string.service));
						break;
					case RunningAppProcessInfo.IMPORTANCE_VISIBLE:
						sets.add(mActivity.getString(R.string.visible));
						break;
					default:
						break;
					}
				}
				StringBuilder buffer = new StringBuilder();
				Iterator<?> it = sets.iterator();
				while (true) {
					buffer.append(it.next());
					if (it.hasNext()) {
						buffer.append(", ");
					} else {
						break;
					}
				}
				return buffer.toString();
			}
		};
	}

}